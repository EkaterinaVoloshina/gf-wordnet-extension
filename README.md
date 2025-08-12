# Extending GF-WordNet by mask-filling 

### Idea 
Model M is given a full sentence in 1-2 languages and a sentence with a masked token and the model has to predict the missed token:

``
the programme was on the air from 9 until midnight
programmet var i sändning från 9 till midnatt

le programme était [MASK] de 9 à du minuit
``

By testing the method on already validated examples, we can extend the GF-WordNet in other languages.

### How to use
`Main.hs` creates datasets for all existing synsets with a masked and non-masked versions of the example (see folder `examples`).

### Possible research questions:
1. Can we filter out "bad", incorrect guesses by looking at models probabilities?
2. Is it enough to give one masked sentence or sentences in other languages help? Do similar or more diverse languages help better?
