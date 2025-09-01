Below is an extended continuation paper that focuses on exploring numerical methods to solve the projectile interception problem, particularly for the constant acceleration case where a quartic equation arises. This paper builds on the previous analyses by delving into specific numerical techniques, providing detailed equations for each method, and including iterative formulas, error estimations, and convergence considerations. The emphasis is on equations, as requested, to offer a mathematically rich exploration of numerical solutions without repeating prior derivations excessively.

# Numerical Methods for Projectile Interception: Solving the Quartic Equation

## Abstract
This paper investigates numerical methods to determine the interception time 
$t^*$
 and projectile velocity 
$\mathbf{v}_\beta$
 for a target moving with constant acceleration, as formulated in prior kinematic analyses. We focus on solving the resulting quartic equation using the Bisection Method, Newton-Raphson Method, and Secant Method, providing detailed equations for each approach. Additional considerations include error analysis, convergence rates, and practical implementation strategies, all supported by a wealth of mathematical formulations.
## 1. Introduction
In the projectile interception problem with a target under constant acceleration 
$\mathbf{a}_\alpha$
, the key challenge is solving a quartic equation to find the time of interception 
$t^*$
. Analytical solutions are impractical due to their complexity, necessitating numerical methods. This paper explores three robust techniques—Bisection, Newton-Raphson, and Secant—each accompanied by extensive equations to compute 
$t^*$
, followed by the derivation of 
$\mathbf{v}_\beta$
. We aim to provide a comprehensive, equation-driven toolkit for numerical resolution.
## 2. Problem Recap: The Quartic Equation
For a target at 
$$ \mathbf{R}_\alpha(t) = \mathbf{R}_{\alpha 0} + \mathbf{v}_{\alpha 0} t + \frac{1}{2} \mathbf{a}_\alpha t^2 $$
 and a projectile at 
$$ \mathbf{R}_\beta(t) = \mathbf{R}_{\beta 0} + \mathbf{v}_\beta t $$
, interception occurs when:
$$ \mathbf{R}_{\alpha 0} + \mathbf{v}_{\alpha 0} t^* + \frac{1}{2} \mathbf{a}_\alpha t^{*2} = \mathbf{R}_{\beta 0} + \mathbf{v}_\beta t^* $$
$$ \mathbf{v}_\beta = \frac{\mathbf{R}_{\alpha 0} - \mathbf{R}_{\beta 0}}{t^*} + \mathbf{v}_{\alpha 0} + \frac{1}{2} \mathbf{a}_\alpha t^* $$
With 
$ \mathbf{r}_0 = \mathbf{R}_{\alpha 0} - \mathbf{R}_{\beta 0} $
 and speed constraint 
$\|\mathbf{v}_\beta\|^2 = s_\beta^2$
:
$$ \left\| \frac{\mathbf{r}_0}{t^*} + \mathbf{v}_{\alpha 0} + \frac{1}{2} \mathbf{a}_\alpha t^* \right\|^2 = s_\beta^2 $$
Expanding:
$$ \frac{\mathbf{r}_0 \cdot \mathbf{r}_0}{t^{*2}} + 2 \frac{\mathbf{r}_0 \cdot \mathbf{v}_{\alpha 0}}{t^*} + \mathbf{r}_0 \cdot \mathbf{a}_\alpha + \mathbf{v}_{\alpha 0} \cdot \mathbf{v}_{\alpha 0} + \mathbf{v}_{\alpha 0} \cdot \mathbf{a}_\alpha t^* + \frac{1}{4} \mathbf{a}_\alpha \ительные \cdot \mathbf{a}_\alpha t^{*2} = s_\beta^2 $$
Multiply by 
t^{*2}
:
$$\frac{1}{4} \mathbf{a}_\alpha \cdot \mathbf{a}_\alpha t^{*^{4}} + \mathbf{v}_{\alpha 0} \cdot \mathbf{a}_\alpha t^{*^{3}} + (\mathbf{r}_0 \cdot \mathbf{a}_\alpha + \mathbf{v}_{\alpha 0} \cdot \mathbf{v}_{\alpha 0} - s_\beta^2) t^{*^{2}} + 2 \mathbf{r}_0 \cdot \mathbf{v}_{\alpha 0} t^* + \mathbf{r}_0 \cdot \mathbf{r}_0 = 0$$
Define coefficients:
$$p_4 = \frac{1}{4} \|\mathbf{a}_\alpha\|^2, \quad p_3 = \mathbf{v}_{\alpha 0} \cdot \mathbf{a}_\alpha, \quad p_2 = \mathbf{r}_0 \cdot \mathbf{a}_\alpha + \|\mathbf{v}_{\alpha 0}\|^2 - s_\beta^2, \quad p_1 = 2 \mathbf{r}_0 \cdot \mathbf{v}_{\alpha 0}, \quad p_0 = \|\mathbf{r}_0\|^2$$
The quartic equation is:
$$ f(t) = p_4 t^4 + p_3 t^3 + p_2 t^2 + p_1 t + p_0 = 0 $$
We seek $t^* > 0$ numerically.
## 3. Bisection Method
### 3.1 Algorithm
The Bisection Method locates a root within an interval 
$ [a, b] $
 where 
$ f(a) f(b) < 0 $
.
Initial bounds: Choose 
$a = 0$
 (since 
$t^* > 0$
), and estimate 
$b$
 (e.g., based on maximum possible 
t^*
):
$ b \approx \frac{\|\mathbf{r}_0\|}{s_\beta - \|\mathbf{v}_{\alpha 0}\|} \quad (\text{if } s_\beta > \|\mathbf{v}_{\alpha 0}\|)$
Evaluate:
$ f(a) = p_0, \quad f(b) = p_4 b^4 + p_3 b^3 + p_2 b^2 + p_1 b + p_0 $
Midpoint:
$ c_n = \frac{a_n + b_n}{2} $
Update rule:
$ \text{If } f(a_n) f(c_n) < 0, \text{ then } b_{n+1} = c_n, \, a_{n+1} = a_n $
$ \text{Else } a_{n+1} = c_n, \, b_{n+1} = b_n $
### 3.2 Iteration Equations
For iteration $n$:
$ f(c_n) = p_4 c_n^4 + p_3 c_n^3 + p_2 c_n^2 + p_1 c_n + p_0 $
Interval length:
$ \Delta_n = b_n - a_n = \frac{b_0 - a_0}{2^n} $
Stopping criterion (tolerance $\epsilon$):
$ \Delta_n < \epsilon \quad \Rightarrow \quad 2^n > \frac{b_0 - a_0}{\epsilon} $
$ n > \log_2 \left( \frac{b_0 - a_0}{\epsilon} \right) $
### 3.3 Error Estimation
Absolute error:
$$|t^* - c_n| \leq \frac{b_n - a_n}{2} = \frac{b_0 - a_0}{2^{n+1}}$$
Relative error (assuming $t^* \approx c_n$):
$$\frac{|t^* - c_n|}{c_n} \leq \frac{b_0 - a_0}{2^{n+1} c_n}$$
## 4. Newton-Raphson Method
### 4.1 Algorithm
The Newton-Raphson Method uses the derivative to refine an initial guess $t_0$:
$$t_{n+1} = t_n - \frac{f(t_n)}{f'(t_n)}$$
Derivative:
$$f'(t) = 4 p_4 t^3 + 3 p_3 t^2 + 2 p_2 t + p_1$$
### 4.2 Iteration Equations
Evaluate at $t_n$:
$$f(t_n) = p_4 t_n^4 + p_3 t_n^3 + p_2 t_n^2 + p_1 t_n + p_0$$
$$f'(t_n) = 4 p_4 t_n^3 + 3 p_3 t_n^2 + 2 p_2 t_n + p_1$$
Update:
$$t_{n+1} = t_n - \frac{p_4 t_n^4 + p_3 t_n^3 + p_2 t_n^2 + p_1 t_n + p_0}{4 p_4 t_n^3 + 3 p_3 t_n^2 + 2 p_2 t_n + p_1}$$
### 4.3 Initial Guess
Estimate $t_0$ using the constant velocity approximation:
$$t_0 = \frac{\|\mathbf{r}_0\|}{s_\beta - \|\mathbf{v}_{\alpha 0}\|}$$
### 4.4 Error Analysis
Taylor expansion around 
t^*
:
$$f(t_n) \approx f(t^*) + f'(t^*) (t_n - t^*) + \frac{1}{2} f''(t^*) (t_n - t^*)^2$$
Since $f(t^*) = 0$:
$$t_{n+1} - t^* \approx t_n - t^* - \frac{f'(t^*) (t_n - t^*) + \frac{1}{2} f''(t^*) (t_n - t^*)^2}{f'(t_n)}$$
$$f''(t) = 12 p_4 t^2 + 6 p_3 t + 2 p_2$$
Error:
$$e_{n+1} = t_{n+1} - t^* \approx \frac{f''(t^*)}{2 f'(t^*)} e_n^2$$
Quadratic convergence rate:
$$|e_{n+1}| \leq k |e_n|^2, \quad k = \left| \frac{f''(t^*)}{2 f'(t^*)} \right|$$
Stopping criterion:
$ |f(t_n)| < \epsilon $
## 5. Secant Method
### 5.1 Algorithm
The Secant Method approximates the derivative using two points:
$ t_{n+1} = t_n - f(t_n) \frac{t_n - t_{n-1}}{f(t_n) - f(t_{n-1})} $
### 5.2 Iteration Equations
For points 
t_{n-1}
 and 
t_n
:
$ f(t_{n-1}) = p_4 t_{n-1}^4 + p_3 t_{n-1}^3 + p_2 t_{n-1}^2 + p_1 t_{n-1} + p_0 $
$ f(t_n) = p_4 t_n^4 + p_3 t_n^3 + p_2 t_n^2 + p_1 t_n + p_0 $
Slope:
$ s_n = \frac{f(t_n) - f(t_{n-1})}{t_n - t_{n-1}} $
Update:
$ t_{n+1} = t_n - \frac{f(t_n)}{s_n} = t_n - f(t_n) \frac{t_n - t_{n-1}}{f(t_n) - f(t_{n-1})} $
Factorized form:
$ t_{n+1} = \frac{t_{n-1} f(t_n) - t_n f(t_{n-1})}{f(t_n) - f(t_{n-1})} $
### 5.3 Initial Guesses
Choose $t_0 = 0$, $t_1 = \frac{\|\mathbf{r}_0\|}{s_\beta}$:
$$f(t_0) = p_0, \quad f(t_1) = p_4 t_1^4 + p_3 t_1^3 + p_2 t_1^2 + p_1 t_1 + p_0$$
### 5.4 Error Analysis
Error relation:
$$e_{n+1} \approx \frac{f''(t^*)}{2 f'(t^*)} e_n e_{n-1}$$
Convergence order $\approx 1.618$ (golden ratio):
$$|e_{n+1}| \leq k |e_n|^{1.618}, \quad k = \left| \frac{f''(t^*)}{2 f'(t^*)} \right|^{0.618}$$
Stopping criterion:
$ |t_{n+1} - t_n| < \epsilon $
## 6. Computing $\mathbf{v}_\beta$
Once
$ t^* $
 is found:
$ v_{\beta x} = \frac{r_{0x}}{t^*} + v_{\alpha 0x} + \frac{1}{2} a_{\alpha x} t^* $
$ v_{\beta y} = \frac{r_{0y}}{t^*} + v_{\alpha 0y} + \frac{1}{2} a_{\alpha y} t^* $
$ v_{\beta z} = \frac{r_{0z}}{t^*} + v_{\alpha 0z} + \frac{1}{2} a_{\alpha z} t^* $
Verification:
$ v_{\beta x}^2 + v_{\beta y}^2 + v_{\beta z}^2 = s_\beta^2 $
## 7. Practical Considerations
### 7.1 Multiple Roots
Evaluate 
f(t)
 at all real, positive roots:
t^*_k = \text{root } k \text{ from method}
Select 
t^* = \min \{ t^*_k > 0 \}
 for earliest interception.
### 7.2 Stability Equations
Check derivative sign:
$ f'(t^*) \neq 0 $
Second derivative for curvature:
$ f''(t^*) = 12 p_4 t^{*2} + 6 p_3 t^* + 2 p_2 $
### 7.3 Iteration Count
Bisection:
$ n \approx \log_2 \left( \frac{b_0 - a_0}{\epsilon} \right) $
Newton-Raphson:
$ n \approx \log_2 \left( \log \left( \frac{\epsilon}{|e_0|} \right) \right) $
  (quadratic).
## 8. Conclusion
This paper has provided an equation-rich exploration of numerical methods to solve the quartic interception equation. The Bisection Method offers reliability, Newton-Raphson provides rapid convergence, and the Secant Method balances efficiency and simplicity. Each method’s equations enable precise computation of 
t^*
 and 
\mathbf{v}_\beta
, enhancing the practical utility of the kinematic framework.
Final Answer: 
\boxed{\mathbf{v}_\beta}
, computed numerically as above.}
This paper maximizes equations by detailing each method’s iterative steps, derivatives, and error analyses, offering a thorough numerical complement to the prior analytical work.