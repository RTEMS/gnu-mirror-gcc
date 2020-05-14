#!/usr/bin/env python3

import os
import re
import sys

from unidiff import PatchSet

pr_regex = re.compile(r'(\/(\/|\*)|[Cc*!])\s+(?P<pr>PR [a-z+-]+\/[0-9]+)')
comment_regex = re.compile(r'^\/\*')
struct_regex = re.compile(r'^((class|struct|union|enum)\s+[a-zA-Z0-9_]+)')
macro_regex = re.compile(r'#\s*(define|undef)\s+([a-zA-Z0-9_]+)')
super_macro_regex = re.compile(r'^DEF[A-Z0-9_]+\s*\(([a-zA-Z0-9_]+)')
fn_regex = re.compile(r'([a-zA-Z_][^()\s]*)\s*\([^*]')
template_and_param_regex = re.compile(r'<[^<>]*>')

function_extensions = set(['.c', '.cpp', '.C', '.cc', '.h', '.inc', '.def'])

changelogs = {}

filename = sys.argv[1]
diff = PatchSet.from_filename(filename)

script_folder = os.path.realpath(__file__)
gcc_root = os.path.dirname(os.path.dirname(script_folder))


def find_changelog(path):
    folder = os.path.split(path)[0]
    while True:
        if os.path.exists(os.path.join(gcc_root, folder, 'ChangeLog')):
            return folder
        folder = os.path.dirname(folder)
        if folder == '':
            return folder
    raise AssertionError()


def extract_function_name(line):
    if comment_regex.match(line):
        return None
    m = struct_regex.search(line)
    if m:
        # Struct declaration
        return m.group(1)
    m = macro_regex.search(line)
    if m:
        # Macro definition
        return m.group(2)
    m = super_macro_regex.search(line)
    if m:
        # Supermacro
        return m.group(1)
    m = fn_regex.search(line)
    if m:
        # Discard template and function parameters.
        fn = m.group(1)
        fn = re.sub(template_and_param_regex, '', fn)
        return fn.rstrip()
    return None


def try_add_function(functions, line):
    fn = extract_function_name(line)
    if fn and fn not in functions:
        functions.append(fn)
    return bool(fn)


sorted_changelogs = []
prs = []

for file in diff:
    changelog = find_changelog(file.path)
    if changelog not in changelogs:
        changelogs[changelog] = []
        sorted_changelogs.append(changelog)
    changelogs[changelog].append(file)

    if 'testsuite' in file.path and file.is_added_file:
        for line in list(file)[0]:
            m = pr_regex.search(line.value)
            if m:
                pr = m.group('pr')
                if pr not in prs:
                    prs.append(pr)
            else:
                break

for changelog in sorted_changelogs:
    files = changelogs[changelog]
    print('%s:' % os.path.join(changelog, 'ChangeLog'))
    print()
    for pr in prs:
        print('\t%s' % pr)
    for file in files:
        assert file.path.startswith(changelog)
        in_tests = 'testsuite' in changelog or 'testsuite' in file.path
        relative_path = file.path[len(changelog):].lstrip('/')
        functions = []
        if file.is_added_file:
            print('\t* %s: %s.' % (relative_path,
                                   'New test' if in_tests else 'New file'))
        elif file.is_removed_file:
            print('\t* %s: Removed.' % (relative_path))
        else:
            for hunk in file:
                # Do not add function names for testsuite files
                extension = os.path.splitext(relative_path)[1]
                if not in_tests and extension in function_extensions:
                    last_fn = None
                    modified_visited = False
                    success = False
                    for line in hunk:
                        m = re.match(r'^([a-zA-Z0-9_#].*)', line.value)
                        if line.is_added or line.is_removed:
                            if not line.value.strip():
                                continue
                            modified_visited = True
                            if m and try_add_function(functions, m.group(1)):
                                last_fn = None
                                success = True
                        elif line.is_context:
                            if last_fn and modified_visited:
                                try_add_function(functions, last_fn)
                                last_fn = None
                                modified_visited = False
                                success = True
                            elif m:
                                last_fn = m.group(1)
                                modified_visited = False
                    if not success:
                        try_add_function(functions, hunk.section_header)
            if functions:
                print('\t* %s (%s):' % (relative_path, functions[0]))
                for fn in functions[1:]:
                    print('\t(%s):' % fn)
            else:
                print('\t* %s:' % relative_path)
    print()
