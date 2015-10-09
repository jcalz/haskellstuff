{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeInferenceQuestions where

class Hash h k i | h -> k i where
    func :: h -> k -> i
    hash :: h
    hash = undefined

data EverythingIsZero a

instance Hash (EverythingIsZero a) a Int where 
    func _ = (const 0)

data StringHash 

instance Hash StringHash String Int where
    func _ = h where
        h [] = 0
        h (c:cs) = ((fromEnum c) + (h cs)*33)


foo = func (hash::StringHash)

goo = func (hash::EverythingIsZero String)

main = do
    print "start"
    print $ foo "heythejakanskanskanaskskjskjskdsrez"
    print $ goo "heythere"
    print "done"