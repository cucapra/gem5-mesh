import logging
import colorlog

# Set up logging with ***colors***.
_handler = colorlog.StreamHandler()
_handler.setFormatter(colorlog.ColoredFormatter(
    '%(log_color)s%(message)s',
    log_colors={
        'DEBUG':    'thin_white',
        'INFO':     'thin_white',
        'WARNING':  'yellow',
        'ERROR':    'red',
        'CRITICAL': 'red',
    },
))
log = colorlog.getLogger('trillium')
log.setLevel(logging.INFO)
log.addHandler(_handler)


class ParseError(Exception):
    """The input assembly programs were in an unexpected form.
    """
