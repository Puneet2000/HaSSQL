# HaSSQL
[![CircleCI token](https://img.shields.io/circleci/token/734a9a6d01b2f9e068ffedcf869c71a598de6ade/project/github/IITH-SBJoshi/haskell-8/master.svg?logo=circleci&style=for-the-badge)](https://circleci.com/gh/IITH-SBJoshi/haskell-8/tree/master)
[![License](https://img.shields.io/badge/License-BSD--3-informational.svg?style=for-the-badge)](https://github.com/IITH-SBJoshi/haskell-8/blob/master/LICENSE)
[![Inline docs](https://img.shields.io/badge/Haddock-Reference-blue.svg?style=for-the-badge&logo=haskell)](http://104.211.220.36/docs/index.html)

Haskell Simple Structured Query Language is a subset of SQL with a local database integration based on Parsec Library.

* [Description](#description)
* [Documentation](#documentation)
* [Setup](#setup)
* [Grammar](#grammar)
* [Usage and Syntax](#usage-and-syntax)
* [Examples](#examples)
* [Guidelines for contribution](#guidelines-for-contribution)
* [External Packages and References](#external-packages-and-references)

## Description
1. **Objective**
	- Understand Functional programming paradigm.
	- Using software technologies like Continous Integration , Unit Testing , Documentation .
2. **Features/Design**
	- Supports ```Select```, ```Create```, ```Insert```, ```Delete``` queries.
	- Supports ```From```, ```Where```, ```ORDER BY``` Clauses.
	- Supports ```Integer```, ```Boolean```, ```String``` Literals.
	- Support aliases in ```Select``` query.
	- A local database (volatile) backend support to perform queries live.
	- A Command Line Interface for easy Usage.
3. **Team**
	- Shraiysh Vaishay (CS17BTECH11050)
	- Puneet Mangla (CS17BTECH11029)
	- Lingam Sai Ramana Reddy (CS17BTECH11022)
	- Hitesh (MA17BTECH11004)
  
## Documentation
  - Present package documentation can be found [here](http://104.211.220.36/docs/index.html).
  - Haddock understands various annotations in the source file, and puts them in the generated docs.
  - Please refer [this link](https://www.haskell.org/haddock/doc/html/markup.html) to know how to comment your source code in order to generate good documentation.
  
 ## Setup
 1. **Installations**
      - Install [Haskell for Linux](https://www.haskell.org/downloads/linux/)
      - Install [stack framework](https://docs.haskellstack.org/en/stable/README/)
 2. **Building and Running tests suites**
      ```
      $ git clone https://github.com/IITH-SBJoshi/haskell-8.git
      $ stack build
      $ stack test
      $ stack run
     ```
 ## Grammar
 ```
 <table_name> or <column_name> or <alias> -> <Iden>
 <Iden> -> valid identifier except reserved keywords like from, where, order etc.
 <datatype> -> INTEGER | STRING | BOOL
 <value> -> <NumLit> | <BoolLit> | <StringLit>
 <BoolLit> -> True | False
 <StringLit> -> valid string in single quotes eg : 'Hello World'
 <NumLit> -> (digit)+
 <value_expression> -> <NumLit> | <BoolLit> | <StringLit> 
 			| <Iden> | (<value_expression>) | * 
				| <value_expression> <BinOp> <value_expression>
				| <PrefOp> <value_expression>
 <PreOp> -> - | + | not 
 <BinOp> -> ^ | * | / | % | + | - | <= | >= | != | < | > | = | and | or
 ```
 ## Usage and Syntax
 1. **Create table**
      -  ```create table <table_name> (<column_name> <datatype> ,<column_name> <datatype> ... )```
 2. **Insert record**
      - ```insert into <table_name> (<column_name>,<column_name> ...) values (<value>, <value> ...)``` 
      - To add enteries in columns by default : ```insert into <table_name> values (<value>, <value> ...)```
 3. **Delete records**
      - ```delete from <table_name> where <value_expression>```
 4. **Select records**
      - ```select <column_name> as <alias>, <column_name> as <alias> ... from <table_name> where <value_expession> order by <value_expession>, <value_expession> ...```
      - ```select <column_name> <alias>, <column_name> <alias> ... from <table_name> where <value_expession> order by <value_expession>, <value_expession> ...```
      - To select all columns : ```select * from <table_name> where <value_expession> order by <value_expession>, <value_expession> ...```
 5. **Exiting** : ```exit```
 6. **Displaying whole database instance** : ```output```
 
 ## Examples
 ```
 1. create table table1 ( c1 INTEGER , c2 STRING , c3 BOOL)
 2. insert into table1 values (1,'Puneet',True)
 3. insert into table1 values (11,'Shraiysh',False)
 4. select * from table1
 5. select * from table1 where c1>5 order by c1*c1 - c1
 6. select c1,c2 from table1 where c3
 7. select c1 as a, c2 as b from table1 where c3
 8. select c1 a, c2 b from table1 where (not c3)
 ```
      
## Guidelines for contribution :
1. Take open issues and ask for assignment in comment section.
2. **Working on seperate branch**
	- Clone the repository : ```git clone https://github.com/IITH-SBJoshi/haskell-8.git```
	- Create a issue specific branch in cloned repository : ```git checkout -b issue#<issue number>```
	- Run the code by following the steps above
	- You can now start working on your current branch
3. **Testing the changes** : Run the test cases if any: ```stack test```
4. **Commiting the changes**
	- Update ```.gitignore``` if there is any need .
	- To add changes in your working directory : ```git add .```
	- Commit your changes : ```git commit -m "<message>"```
	- Follow a simple commit message guideline eg . ``` Fix <issue_id> : <small description> Author@<your name>```
5. **Pushing the changes**
	- Get current master: `git fetch origin master`
	- Merge master with your branch: `git merge master`
	- Push your changes : ```git push origin <your branch name>:<your branch name>```
	- Make sure that ```Circle CI build``` is passed.
6. **Generating Pull requests :**
	- [Generate a pull request](https://help.github.com/articles/about-pull-requests/) from your ```branch``` branch to ```master``` branch.
	- Give the PR and apt title, and mention `Fixes #<issue_number>` in the comment to link it with the issue.
	- Don't close the issue by your own.
7. **Commenting your Code**
	- Haddock understands various annotations in the source file, and puts them in the generated docs.
	- Please refer [this link](https://www.haskell.org/haddock/doc/html/markup.html) to know how to comment your source code in order to generate good documentation.

## External Packages and References:
- [Introduction to Parsing with Parsec Haskell](http://jakewheat.github.io/intro_to_parsing/)
- [Parsec Haskell](https://github.com/haskell/parsec)
- [Introduction to Haskell](http://learnyouahaskell.com/)
 


