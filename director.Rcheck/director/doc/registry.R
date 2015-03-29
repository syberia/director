## ----, echo = FALSE, message = FALSE-------------------------------------
library(director)

## ------------------------------------------------------------------------
r <- registry$new(file.path(dirname(tempfile()), '.registry'))
# You don't have to use .registry, but making the directory hidden is preferable
# as it is for internal use only.

r$set('some/key', list('some', 5, 'value'))
value <- r$get('some/key') # value is now the above list.
print(value)

## ------------------------------------------------------------------------
projects <- list(proj1 = list(config_key1 = 'config_value1', config_key2 = 'config_value2'),
                 proj2 = list(config_key1 = 'settings_value1', config_key2 = 'settings_value2'))
for (name in names(projects)) r$set(file.path(name, 'config'), projects[[name]])
print(sapply(names(projects), function(name) r$get(file.path(name, 'config'))$config_key1))

