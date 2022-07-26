# GCC Sphinx customization

__version__ = '1.0'

def setup(app):
    app.add_object_type('gcc-attr', 'gcc-attr', objname='GCC attribute',
                        indextemplate='pair: %s; attribute')
    app.add_object_type('gcc-param', 'gcc-param', objname='GCC parameter',
                        indextemplate='pair: %s; parameter')

    return dict(
        version = __version__,
        parallel_read_safe = True,
        parallel_write_safe = True
    )
