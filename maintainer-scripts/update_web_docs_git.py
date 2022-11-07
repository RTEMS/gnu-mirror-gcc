#!/usr/bin/env python3

# Generate documentation from Sphinx files.

import argparse
import os
import shutil
import subprocess
import tempfile
from pathlib import Path

GITROOT = '/git/gcc.git'
BUGURL = 'https://gcc.gnu.org/bugs/'

parser = argparse.ArgumentParser(description='Update web documentation.')
parser.add_argument('output_folder', help='Output folder')
parser.add_argument('--gitrepo', help=f'Git repository (default: {GITROOT})',
                    default=GITROOT)
parser.add_argument('--sphinx-build', help='Path to sphinx-build binary')
args = parser.parse_args()


def find_configs():
    for root, _, files in os.walk('.'):
        for f in files:
            full = os.path.join(root, f)
            if f == 'conf.py':
                # find name of the documentation
                lines = open(full).read().splitlines()
                docname = None
                for line in lines:
                    if line.startswith('name = '):
                        docname = line.split()[-1].strip("'")
                        break
                assert docname
                yield (Path(root).resolve(), docname)


with tempfile.TemporaryDirectory() as folder:
    print(f'Using {folder} as temporary directory')
    os.chdir(folder)
    subprocess.check_output(f'git clone {args.gitrepo} --depth=1', shell=True)
    # TODO: remove
    os.chdir('gcc')
    cmd = 'git fetch origin refs/users/marxin/heads/sphinx-final --depth=1'
    subprocess.check_output(cmd, shell=True)
    subprocess.check_output('git checkout FETCH_HEAD', shell=True)
    configs = list(find_configs())

    # Prepare folders
    output = Path(args.output_folder)
    if not output.exists():
        output.mkdir()

    temp = Path('tmp').resolve()
    temp.mkdir()

    # Prepare default env. variables
    childenv = os.environ.copy()
    childenv['BUGURL'] = BUGURL

    # Build and copy the documentation
    for config_folder, docname in sorted(configs):
        print('=== BUILDING:', config_folder, docname, '===')

        # Build HTML
        cmd = f'make -C doc html SOURCEDIR={config_folder} BUILDDIR={temp}/{docname}'
        if args.sphinx_build:
            cmd += f' SPHINXBUILD={args.sphinx_build}'
        subprocess.check_output(cmd, shell=True, env=childenv)
        os.unlink(f'{temp}/{docname}/html/.buildinfo')
        shutil.copytree(f'{temp}/{docname}/html', f'{output}/{docname}',
                        dirs_exist_ok=True)

        # Build PDF
        cmd = f'make -C doc latexpdf SOURCEDIR={config_folder} BUILDDIR={temp}/pdf/{docname}'
        if args.sphinx_build:
            cmd += f' SPHINXBUILD={args.sphinx_build}'
        subprocess.check_output(cmd, shell=True, env=childenv)
        shutil.copyfile(f'{temp}/pdf/{docname}/latex/{docname}.pdf',
                        f'{output}/{docname}.pdf')
