

download.file('http://nlp.stanford.edu/data/glove.6B.zip', 
              destfile = './glove.6B.zip')

unzip('./week_9/glove.6B.zip',
      exdir='./glove_models') 


download.file('http://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz', 
              destfile = './week_9/aclImdb_v1.tar.gz')

untar('./week_9/aclImdb_v1.tar.gz',
      exdir='./imbd_data')
