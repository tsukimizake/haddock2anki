module Types where

type Name = String
type Doc = String
type Signature = String
type Method = String
type Methods = [Method]
type Instances = [Item]
type Source = String
type Fixity = String

data Item = Data Name Doc Source
          | Class Name Doc Methods Instances Source
          | Op Name Signature Doc Fixity Source
          | Func Name Signature Doc Source
          deriving (Show, Read, Eq)
