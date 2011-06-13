\chapter*{Abstract}

The field of physical modelling and simulation plays a vital role in advancing
numerous scientific and engineering disciplines. To cope with the increasing
size and complexity of physical models, a number of modelling and simulation
languages have been developed. These languages can be divided into two broad
categories: causal and noncausal. Causal languages express a system model in
terms of explicit equations. In contrast, a noncausal model is formulated in
terms of implicit equations. The fact that the causality can be left implicit
makes noncausal languages more declarative and noncausal models more reusable.
These are considered to be crucial advantages in many physical domains.

Current, main-stream noncausal languages do not treat models as first-class
values; that is, a model cannot be parametrised on other models or generated
at simulation runtime. This results in very limited higher-order and
structurally dynamic modelling capabilities, and limits the expressiveness and
applicability of noncausal languages.

This thesis is about a novel approach to the design and implementation of
noncausal languages with first-class models supporting higher-order and
structurally dynamic modelling. In particular, the thesis presents a language
that enables: (1) higher-order modelling capabilities by embedding noncausal
models as first-class entities into a functional programming language and (2)
efficient simulation of noncausal models that are generated at simulation
runtime by runtime symbolic processing and just-in-time compilation. These
language design and implementation approaches can be applied to other
noncausal languages. This thesis provides a self-contained reference for such
an undertaking by defining the language semantics formally and providing an
in-depth description of the implementation. The language provides noncausal
modelling and simulation capabilities that go beyond the state of the art, as
backed up by a range of examples presented in the thesis, and represents a
significant progress in the field of physical modelling and simulation.