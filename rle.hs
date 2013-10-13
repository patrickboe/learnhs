import Data.List

encode s = map summarize (group s)
  where summarize g = (length g , head g)

decode code = concat (map extemporize code)
  where extemporize (o,c) = replicate o c
