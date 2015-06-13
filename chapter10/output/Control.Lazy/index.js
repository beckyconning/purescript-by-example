// Generated by psc-make version 0.6.9.3

/**
 *  | This module defines the `Lazy` type class and associated
 *  | helper functions.
 */
"use strict";
var Prelude = require("Prelude");

/**
 *  | The `Lazy` class represents types which allow evaluation of values
 *  | to be _deferred_.
 *  |
 *  | Usually, this means that a type contains a function arrow which can
 *  | be used to delay evaluation.
 */
var Lazy = function (defer) {
    this.defer = defer;
};

/**
 *  | A version of `Lazy` for type constructors of one type argument.
 */
var Lazy1 = function (defer1) {
    this.defer1 = defer1;
};

/**
 *  | A version of `Lazy` for type constructors of two type arguments.
 */
var Lazy2 = function (defer2) {
    this.defer2 = defer2;
};

/**
 *  | A version of `Lazy` for type constructors of two type arguments.
 */
var defer2 = function (dict) {
    return dict.defer2;
};

/**
 *  | A version of `fix` for type constructors of two type arguments.
 */
var fix2 = function (__dict_Lazy2_0) {
    return function (f) {
        return defer2(__dict_Lazy2_0)(function (_11) {
            return f(fix2(__dict_Lazy2_0)(f));
        });
    };
};

/**
 *  | A version of `Lazy` for type constructors of one type argument.
 */
var defer1 = function (dict) {
    return dict.defer1;
};

/**
 *  | A version of `fix` for type constructors of one type argument.
 */
var fix1 = function (__dict_Lazy1_1) {
    return function (f) {
        return defer1(__dict_Lazy1_1)(function (_10) {
            return f(fix1(__dict_Lazy1_1)(f));
        });
    };
};

/**
 *  | The `Lazy` class represents types which allow evaluation of values
 *  | to be _deferred_.
 *  |
 *  | Usually, this means that a type contains a function arrow which can
 *  | be used to delay evaluation.
 */
var defer = function (dict) {
    return dict.defer;
};

/**
 *  | `fix` defines a value as the fixed point of a function.
 *  |
 *  | The `Lazy` instance allows us to generate the result lazily.
 */
var fix = function (__dict_Lazy_2) {
    return function (f) {
        return defer(__dict_Lazy_2)(function (_9) {
            return f(fix(__dict_Lazy_2)(f));
        });
    };
};
module.exports = {
    Lazy2: Lazy2, 
    Lazy1: Lazy1, 
    Lazy: Lazy, 
    fix2: fix2, 
    fix1: fix1, 
    fix: fix, 
    defer2: defer2, 
    defer1: defer1, 
    defer: defer
};