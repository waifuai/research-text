# White Paper: Projectile Interception of Moving Targets: A Kinematic Analysis

**Abstract:**

This paper explores the problem of determining the launch parameters of a projectile designed to intercept a moving target, both under conditions of constant velocity and constant acceleration. We employ a kinematic analysis, utilizing differential equations to model the motion of both the projectile and the target. We develop systems of equations describing the intersection conditions and provide closed-form solutions for the constant velocity case, while highlighting the need for numerical approaches for the acceleration scenario. Practical considerations and limitations in real-world applications are also discussed.

**1. Introduction**

The interception of moving targets by projectiles is a fundamental problem in classical mechanics with applications ranging from ballistic trajectory calculations to the design of automated interception systems. While simplifying assumptions, such as neglecting drag and other external forces, are common for initial analysis, a deep understanding of the kinematic principles governing the projectile-target interaction is essential. This paper presents a detailed mathematical framework for this interception problem, first addressing the case of targets with constant velocity and then extending it to targets undergoing constant acceleration.

**2. Theoretical Framework: Constant Target Velocity**

Consider a projectile ('b') launched to intercept a target ('a'). We define the following position vectors:

*   $\mathbf{r_a}(t) = \mathbf{r_{a0}} + \mathbf{v_a} t$ : Target's position as a function of time.
*   $\mathbf{r_b}(t) = \mathbf{r_{b0}} + \mathbf{v_b} t$: Projectile's position as a function of time.

Where:
* $\mathbf{r_{a0}} = (x_a, y_a, z_a)$ is the initial position of target 'a'.
*  $\mathbf{v_a} = (v_{ax}, v_{ay}, v_{az})$ is the constant velocity of target 'a'.
* $\mathbf{r_{b0}} = (x_b, y_b, z_b)$ is the initial position of projectile 'b'.
* $\mathbf{v_b} = (v_{bx}, v_{by}, v_{bz})$ is the initial velocity of projectile 'b', which we seek to determine.
* $t$ is the time elapsed from the start.

The projectile has a fixed speed $s_b$ such that
$$\|\mathbf{v_b}\|^2 =  v_{bx}^2 + v_{by}^2 + v_{bz}^2 = s_b^2$$

The interception occurs at some time $t^*$ when the projectile and target occupy the same position:
$$\mathbf{r_a}(t^*) = \mathbf{r_b}(t^*)$$
This leads to three equations:


$x_a + v_{ax}t^* = x_b + v_{bx}t^*$  (1)

$y_a + v_{ay}t^* = y_b + v_{by}t^*$  (2)

$z_a + v_{az}t^* = z_b + v_{bz}t^*$  (3)


From equation (1) we obtain the time of intersection:
$$t^* = \frac{x_b - x_a}{v_{ax} - v_{bx}}$$

Substituting the time of intersection to the remaining two equations we obtain:


$y_a + v_{ay} \frac{x_b - x_a}{v_{ax} - v_{bx}} = y_b + v_{by} \frac{x_b - x_a}{v_{ax} - v_{bx}}$

$z_a + v_{az} \frac{x_b - x_a}{v_{ax} - v_{bx}} = z_b + v_{bz} \frac{x_b - x_a}{v_{ax} - v_{bx}}$


These, in conjunction with the speed constraint, form a system of three non-linear equations for three unknowns, $v_{bx}, v_{by}, v_{bz}$. By solving this system, the projectile's velocity vector can be explicitly determined. The solution often contains two possible sets of $v_b$, and therefore two potential launch trajectories. A detailed analysis of the closed-form solution can be found in the Appendix.

**3. Theoretical Framework: Constant Target Acceleration**

Now, let's consider the more complex scenario where the target undergoes constant acceleration. We define the following:

*   $\mathbf{r_a}(t) = \mathbf{r_{a0}} + \mathbf{v_{a0}} t + \frac{1}{2}\mathbf{a_a} t^2$: Target's position vector with constant acceleration.
*   $\mathbf{r_b}(t) = \mathbf{r_{b0}} + \mathbf{v_b} t$: Projectile's position vector.

Here, $\mathbf{a_a} = (a_{ax}, a_{ay}, a_{az})$ represents the constant acceleration of the target. The interception condition remains that the projectile and target are at the same location at time $t^*$:
$$\mathbf{r_a}(t^*) = \mathbf{r_b}(t^*)$$

This yields the following system of equations:


$x_a + v_{ax}t^* + 1/2*a_{ax}t^{*2} = x_b + v_{bx}t^*$    (4)

$y_a + v_{ay}t^* + 1/2*a_{ay}t^{*2} = y_b + v_{by}t^*$    (5)

$z_a + v_{az}t^* + 1/2*a_{az}t^{*2} = z_b + v_{bz}t^*$    (6)

$v_{bx}^2 + v_{by}^2 + v_{bz}^2 = s_b^2$   (7)


Unlike the constant velocity case, these four equations, though mathematically well-posed, cannot be solved analytically for a closed-form solution of $\mathbf{v_b}$ and $t^*$. However, we can reduce the equations by isolating the velocity components to the left-hand side:


$v_{bx}t^* = x_a - x_b + v_{ax}t^* + 1/2*a_{ax}t^{*2}$

$v_{by}t^* = y_a - y_b + v_{ay}t^* + 1/2*a_{ay}t^{*2}$

$v_{bz}t^* = z_a - z_b + v_{az}t^* + 1/2*a_{az}t^{*2}$


Then, substituting these into equation (7), the velocity components can be removed and we arrive at a single equation on the unknown $t^*$:

$$(x_a - x_b + v_{ax}t^* + 1/2*a_{ax}t^{*2})^2 + (y_a - y_b + v_{ay}t^* + 1/2*a_{ay}t^{*2})^2 + (z_a - z_b + v_{az}t^* + 1/2*a_{az}t^{*2})^2 = s_b^2 t^{*2}$$

This is a quartic equation in $t^*$ and is difficult to solve analytically.

**4. Numerical Methods**

Due to the complexity of the quartic equation in the acceleration case, numerical methods are needed to find the time of intersection $t^*$. Suitable methods include:

*   **Newton-Raphson Method:** An iterative technique that uses the derivative of the equation to refine an initial estimate for $t^*$.
*   **Bisection Method:** A method that repeatedly halves an interval containing a solution, guaranteeing convergence to a root.

Once a suitable $t^*$ is found, it can be substituted back into equations (4), (5) and (6)  to determine the corresponding velocity components of the projectile.

**5. Practical Considerations and Limitations**

*   **Multiple Solutions:** In both constant velocity and acceleration cases, multiple solutions may exist. Physical considerations, such as minimizing travel time or required launch energy, can be used to select the appropriate solution.
*   **Computational Cost:** Numerical methods can be computationally expensive, especially when high accuracy is required. This needs to be balanced against real-time constraints in dynamic applications.
*   **External Forces:** In real-world scenarios, drag and other forces can significantly influence projectile trajectories. These forces must be accounted for when high precision is required, potentially requiring more complex modeling and simulations.
*   **Target Maneuvers:** This analysis assumes the target’s acceleration is constant. If a target’s acceleration changes dynamically, real-time recalculations are necessary for successful interception.
* **No Solution:** It is possible that there is no solution for 't' that satisfies the equation. This would mean that it is impossible for the projectile to hit the target.

**6. Conclusion**

This paper presents a detailed kinematic analysis for the projectile interception of moving targets. Analytical solutions can be found in the constant target velocity case; however, a numerical approach is required when the target is under constant acceleration. While real-world scenarios introduce complexities such as drag and target maneuvers, this framework provides a strong foundation for understanding and developing practical interception strategies.

**Appendix: Explicit Solution for Constant Target Velocity**

The solution for the projectile velocity components with a constant target velocity is complex but can be solved analytically. The explicit forms for $v_{bx}$, $v_{by}$ and $v_{bz}$ are:


$v_{bx} = (v_{ax} (y_{a} - y_{b})^2 + v_{ax} (z_{a} - z_{b})^2 - v_{ay} (x_{a} - x_{b})(y_{a} - y_{b}) - v_{az} (x_{a} - x_{b})(z_{a} - z_{b}) \pm (x_{a} - x_{b}) sqrt(Δ))/D$

$v_{by} = (-v_{ax} (x_{a} - x_{b})(y_{a} - y_{b}) + v_{ay} (x_{a} - x_{b})^2 + v_{ay} (z_{a} - z_{b})^2 - v_{az} (y_{a} - y_{b})(z_{a} - z_{b}) \pm (y_{a} - y_{b}) sqrt(Δ))/D$

$v_{bz} = (- v_{ax} (x_{a} - x_{b})(z_{a} - z_{b}) - v_{ay} (y_{a} - y_{b})(z_{a} - z_{b}) + v_{az} (x_{a} - x_{b})^2 + v_{az} (y_{a} - y_{b})^2 \pm (z_{a} - z_{b}) sqrt(Δ))/D$

where
$$ \Delta = s_{b}^{2} ((x_{a} - x_{b})^2 + (y_{a} - y_{b})^2 + (z_{a} - z_{b})^2) - (v_{ax} (y_{a} - y_{b}) + v_{ay} (x_{a} - x_{b}) + v_{az} (z_{a} - z_{b}))^2 $$
and
$$ D = (x_{a} - x_{b})^2 + (y_{a} - y_{b})^2 + (z_{a} - z_{b})^2 $$

This solution corresponds to the two possible trajectories for hitting the target, as given by the $\pm$ sign.
