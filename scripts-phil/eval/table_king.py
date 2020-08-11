'''
  Makes the contents of a latex table. Doesn't do any of the formatting stuff, that's left to the doc
'''

# take 2d array and make latex table data
def make_table(data_table, filename):
  outStr = ''
  for row in data_table:
    for i in range(len(row)):
      outStr += ' {} '.format(str(row[i]))
      if (i == len(row) - 1):
        outStr += '\\\\\n'
      else:
        outStr += '&'
  print(outStr)

  with open(filename, 'w+') as fout:
    fout.write(outStr)