(ns clojure-algorithms.maths.first-fundamental-form
  "Calculates the First Fundamental Form of a parametric surface.\n\nThe first fundamental form allows the measurement of lengths, angles,\nand areas on a surface defined by parametric equations r(u, v).\n\nReference:\n- https://en.wikipedia.org/wiki/First_fundamental_form\n\nAuthor: Chahat Sandhu\nGitHub: https://github.com/singhc7")

(declare first-fundamental-form)

(defn first-fundamental-form "Calculate the First Fundamental Form coefficients (E, F, G) for a surface\ndefined by parametric equations x(u,v), y(u,v), z(u,v).\n\nArgs:\n    x_expr: A string representing the x component in terms of u and v.\n    y_expr: A string representing the y component in terms of u and v.\n    z_expr: A string representing the z component in terms of u and v.\n\nReturns:\n    A tuple containing the sympy expressions for E, F, and G.\n\nExamples:\n    >>> # Example 1: A simple plane r(u, v) = <u, v, 0>\n    >>> E, F, G = first_fundamental_form(\"u\", \"v\", \"0\")\n    >>> print(f\"E: {E}, F: {F}, G: {G}\")\n    E: 1, F: 0, G: 1\n\n    >>> # Example 2: A paraboloid r(u, v) = <u, v, u**2 + v**2>\n    >>> E, F, G = first_fundamental_form(\"u\", \"v\", \"u**2 + v**2\")\n    >>> print(f\"E: {E}, F: {F}, G: {G}\")\n    E: 4*u**2 + 1, F: 4*u*v, G: 4*v**2 + 1\n\n    >>> # Example 3: A cylinder r(u, v) = <cos(u), sin(u), v>\n    >>> E, F, G = first_fundamental_form(\"cos(u)\", \"sin(u)\", \"v\")\n    >>> print(f\"E: {sp.simplify(E)}, F: {F}, G: {G}\")\n    E: 1, F: 0, G: 1"
  [& _args]
  ;; stub: original requires numpy-attr:dot,sp; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:dot,sp" {:type :UnsupportedOperation})))

;; NOTE: first_fundamental_form stubbed (numpy-attr:dot,sp)
