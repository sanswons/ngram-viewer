library(tm)
library(ngram)
library(ggplot2)

extract_texts = function(path){
  filenames = list.files(path)
  
  # Create a dataframe to store texts for an all years
  texts_df = data.frame(year = character(), text = character() , stringsAsFactors = FALSE)
  
  # Go through all folders
  for (i in 1:length(filenames)) 
  {
    # Check if it is an year
    if (is.na(as.numeric(filenames[i]))==FALSE)
    {
      year = filenames[i]
      path = paste(c('task/', year,  '/'), collapse = '')
      textnames = list.files(path)
      
      for (textname in textnames) 
      {
        fulltext = ''
        
        # Check if it is a text file
        if (endsWith(textname,'.txt'))
        {
          path = paste(c('task/', year, '/', textname),collapse = '')
          
          # Clean the text 
          text = cleaning(scan(path, what = 'character'))
          
          # combine texts seperated by colon
          fulltext = paste(c(fulltext, text), collapse = ';')
        }
      }
      newRow = data.frame(year, fulltext, stringsAsFactors = FALSE)
      texts_df = rbind(texts_df, newRow)
    }
  }
  print('Created df')
  return(texts_df)
  
}
      

cleaning = function(text){
  text = concatenate(text, collapse = ' ')
  
  # Uniform case
  text = tolower(text)
  
  #Remove urls
  text = gsub('http\\S+\\s*', ',', text)
  
  #Remove punctuation
  text = gsub("[[:punct:]]", "", text)
  
  #Remove extra spaces
  text = gsub('\\s+',' ', text)
  return (text)
}

create_ngrams_allbooks = function(df, max_ngrams){
  nmax = max_ngrams
  
  for (i in 1:length(df$year))
  {
    for (j in 1:nmax)
    {
      # finds all n-grams of length n
      output = ngram(str = df[i,2], n = j)
      
      # Creates a table with counts of n-grams
      table = get.phrasetable(output)
      
      # Saves as a csv in a folder called ngrams
      filename = paste(c('ngrams/',df[i,1],'_',j),collapse = '')
      write.csv(as.data.frame(table), file = filename)
    }
  }
  print('Created csv files')
}

search_ngrams = function(phrase, nyears, filenames){
  print('Searching')
  
  #Pre-process phrase to lower case and trim spaces
  phrase = tolower(paste(c(trimws(phrase), ' '),collapse = ''))
  
  #Remove punctuation
  phrase = gsub("[[:punct:]]", "", phrase)
  
  #Remove extra spaces
  phrase = gsub('\\s+',' ', phrase)
  
  # find the n-gram length
  num_words = as.character(length(strsplit(phrase,split = " ")[[1]]))
  
  # Initialize a dataframe to store all frequencies
  # This dataframe contains the phrase, year, frequency
  freqs_allbooks = data.frame()
  
  for (i in 1:length(nyears))
  {
    # Check for the required csv
    path = paste(c('ngrams/',nyears[i],'_',num_words), collapse = '')
    freqs = read.csv(path)
    freqs$ngrams = as.character(freqs$ngrams)
    
    # Find the index of the n-gram 
    index = match(phrase,freqs$ngrams,nomatch = 0)
    if (index==0)
    {
      frequency = 0 
    }
    else
    {
      frequency = freqs$freq[index]  
    }
    newRow = data.frame(year = nyears[i], freq = frequency, phrase = phrase, stringsAsFactors = FALSE)
    freqs_allbooks = rbind(freqs_allbooks,newRow)
  }
  return(freqs_allbooks)
}

create_plots = function(query){
  filenames = list.files('ngrams/')
  
  # Split the search query
  phrases = strsplit(query,split = ",")
  print('Started')
  freqs = data.frame()
  for (i in 1:length(phrases[[1]])){
    
    # Create frequency table for a phrase
    newfreqs = search_ngrams(phrases[[1]][i], texts_df$year, filenames)
    
    # Bind the frequency tables of all phrases
    freqs = rbind(freqs, newfreqs)
  }
  
  # Plot the frequencies
  p = ggplot(data = freqs, aes( x = year, y = freq, colour = phrase, group = phrase)) + geom_point(size = 3) + geom_line(size = 1)
  return (p)
}

texts_df = extract_texts('task/')
create_ngrams_allbooks(texts_df, max_ngrams = 5)


