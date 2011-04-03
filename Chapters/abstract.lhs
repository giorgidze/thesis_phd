\chapter*{Abstract}

The field of modelling and simulation of physical systems is of great
importance in numerous areas of science and engineering. To cope with ever
increasing size and complexity of real-world systems, a number of modelling
and simulation languages have been developed. These languages can be divided
in two broad categories: causal and non-causal.

Causal languages model a system behaviour in terms of explicit equations
(e.g., ordinary differential equations in explicit form). In contrast, a
non-causal model is formulated in terms of implicit equations (e.g.,
differential algebraic equations in implicit form). The fact that the
causality can be left implicit makes non-causal languages more declarative and
non-causal models more reusable.

Current main-stream non-causal languages are not treating models as
first-class values. That is, a model can not be parametrised on other models
and a new model can not be generated at simulation runtime. This results in a
very limited higher-order and structurally-dynamic modelling capabilities, and
limits the expressiveness and applicability of non-causal languages. This
thesis is about a novel approach to the design and implementation of
non-causal modelling and simulation languages with first-class models
supporting higher-order modelling and structurally-dynamic simulation.

In particular, (1) we enable higher-order modelling capabilities by embedding
non-causal models as a first-class entities into a functional programming
language and (2) enable efficient simulation of non-causal models that are
generated at simulation runtime by developing an extensible runtime symbolic
processor and a just-in-time compiler. This provides non-causal modelling and
simulation capabilities that go beyond the state of the art and represents a
significant progress in the field of design and implementation of declarative
modelling and simulation languages.