#lang racket

(struct person (name age grade))

(define person1 (person "John" 16 9))

(person-name person1)

(person-age person1)

(person-grade person1)