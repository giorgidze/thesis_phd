\chapter*{Abstract}

The field of physical modelling and simulation plays vital role in advancing
numerous scientific and engineering disciplines. To cope with increasing size
and complexity of physical models, a number of modelling and simulation
languages have been developed. These languages can be divided in two broad
categories: causal and non-causal. Causal languages express a system behaviour
in terms of explicit equations. In contrast, a non-causal model is formulated
in terms of implicit equations. The fact that the causality can be left
implicit makes non-causal languages more declarative and non-causal models
more reusable. This is considered as a crucial advantage in many physical
domains.

Current, main-stream non-causal languages are not treating models as
first-class values. That is, a model can not be parametrised on other models
or generated at simulation runtime. This results in a very limited
higher-order and structurally-dynamic modelling capabilities, and limits the
expressiveness and applicability of non-causal languages.

This thesis is about a novel approach to the design and implementation of
non-causal languages with first-class models supporting higher-order and
structurally-dynamic modelling. In particular, the thesis presents a language
that enables: (1) higher-order modelling capabilities by embedding non-causal
models as first-class entities into a functional programming language and (2)
efficient simulation of non-causal models that are generated at simulation
runtime by runtime symbolic processing and just-in-time compilation. These
language design and implementation approaches can be applied to other
non-causal languages. This thesis provides a self-contained reference for such
undertaking by defining the language semantics formally and providing an
in-depth description of the implementation. The language provides non-causal
modelling and simulation capabilities that go beyond the state of the art, as
backed up by a range of examples presented in the thesis, and represents a
significant progress in the field of physical modelling and simulation.