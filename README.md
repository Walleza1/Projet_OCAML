# Modules description

## General comments

This project implements a max-flow-min-cost algorithm based on Busacker-Gowen's, and applies it to the bipartial assignment problem.  
The context is to manage the assignment of students in sport groups, each student making 3 choices by order of preference.   
The algorithm then tries to assign every student to a group based on their preference.

Below you'll find a more precise description of the different modules.

## Gfile, Graph

These modules were the base of our project, mainly given by our teacher. Minimal addings were made at the beginning.

## Ff

Core file, contains implementation of the algorithm   
(note : there is a "classical" version of the Ford Fulkerson algorithm in the gitlog, we built Busacker Gowen on its base)

## Ftest

General purpose test file, takes a graph file as an input, returns flow value and create dot & png result files

## Test

Basic uitary tests : see Test_Graphs/README.md