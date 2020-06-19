Trillium Gluer
==============

To use this, first install the dependencies:

    pip3 install --user -r requirements.txt

Then, to glue an assembly file:

    python3 glue.py [-o <out>.s] <func> <vector>.s <scalar>.s

If no output file is specified with `-o`, the result is printed to standard output.
