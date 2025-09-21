# ContextCoin (CTX) Formal Specification

This document provides a formal specification for the ContextCoin (CTX) smart contract on the Solana blockchain. The goal is to precisely define the program's behavior, state transitions, and invariants to allow for rigorous analysis, testing, and formal verification.

## State Space

The state of the system is defined by a tuple $S = (O, M, T, S_s, B, C, E, R)$, where:

*   $O$: The public key of the ICO owner. ($O \in Pubkey$)
*   $M$: The public key of the SPL token mint. ($M \in Pubkey$)
*   $T$: The total supply of CTX tokens. ($T \in \mathbb{N}$)
*   $S_s$: The number of CTX tokens sold during the ICO. ($S_s \in \mathbb{N}$)
*   $B$: The base price of a CTX token (in lamports). ($B \in \mathbb{N}$)
*   $C$: The scaling factor for the bonding curve. ($C \in \mathbb{N}$)
*   $E$: The public key of the escrow account. ($E \in Pubkey$)
*  $R$: Set of Resource Definitions. ($R \subset \{R_i\}$)
    * $R_i$: A resource.  $R_i = (id, s_a, a_f)$
        * $id$: The unique identifier of the resource. ($id \in String$)
        * $s_a$: The public key of the server which is providing the resource. ($s_a \in Pubkey$)
        * $a_f$: The access fee in lamports to access the resource. ($a_f \in \mathbb{N}$)


## Invariants

The following system-wide invariants must always hold true.

1.  $0 \leq S_s \leq T$: The number of CTX tokens sold must always be within the bounds of total supply.
2.  The escrow balance is always greater than or equal to 0.

## Instructions and State Transitions

### `InitializeIco`

*   **Pre-conditions:** The ICO has not been initialized.
*   **Input:** $m \in Pubkey$, $t \in \mathbb{N}$, $b \in \mathbb{N}$, $c \in \mathbb{N}$
*   **State Transition:**
    $$ S' = (O_n, M_n, T_n, S_{s_n}, B_n, C_n, E_n, R_n) $$
    where:
    *   $O_n = owner\_public\_key$
    *   $M_n = m$
    *   $T_n = t$
    *   $S_{s_n} = 0$
    *   $B_n = b$
    *   $C_n = c$
    *   $E_n = escrow\_public\_key$
    *   $R_n = \emptyset$
*   **Post-conditions:** State $S'$ is created, and all conditions within invariant 1 hold true.

### `BuyTokens`

*   **Pre-conditions:** The ICO has been initialized.
*   **Input:** $a \in \mathbb{N}$ (The lamports being paid)
*   **Helper Function:**
    $$
    p =
    \begin{cases}
        b \times (1 + \lfloor S_s / c\rfloor) & \text{if }  \lfloor S_s / c\rfloor > 0 \\
        b & \text{if }  \lfloor S_s / c\rfloor = 0
    \end{cases}
    $$
*  **Tokens to Mint:** $t_m = \lfloor a / p \rfloor $ (the number of tokens to mint)
*   **State Transition:**
    $$ S' = (O, M, T, S_{s_n}, B, C, E, R) $$
    where:
    *   $S_{s_n} = S_s + t_m$
*   **Post-conditions:** $S_s < T$ and escrow account balance increases by $a$, tokens are minted to the buyer, all invariants hold true.

### `SellTokens`

*   **Pre-conditions:** The ICO has been initialized.
*  **Input:** $a \in \mathbb{N}$ (The number of tokens to sell)
*   **Helper Function:**
    $$
    p =
    \begin{cases}
        b \times (1 + \lfloor (S_s - a) / c \rfloor) & \text{if }  \lfloor (S_s - a) / c \rfloor > 0 \\
        b & \text{if }  \lfloor (S_s - a) / c \rfloor = 0
    \end{cases}
    $$
*   **Tokens to Burn:** $t_b = a$
*   **SOL to Return:** $s_r = p * t_b$
*   **State Transition:**
    $$ S' = (O, M, T, S_{s_n}, B, C, E, R) $$
        where:
    *   $S_{s_n} = S_s - t_b $
*   **Post-conditions:** $S_s \geq 0$, tokens are burned, the seller receives $s_r$ lamports from escrow, all invariants hold true.

### `WithdrawFromEscrow`

*   **Pre-conditions:** The ICO has been initialized.
*   **Input:** $a \in \mathbb{N}$ (The amount to withdraw in lamports).
*   **State Transition:**
    $$ S' = S $$
*   **Post-conditions:** The owner receives $a$ lamports from the escrow account, and all invariants hold true.

### `CreateResourceAccess`

*   **Pre-conditions:** The resource state does not exist.
*   **Input:**  $id \in String$, $s_a \in Pubkey$, $a_f \in \mathbb{N}$
*   **State Transition:**
    $$ S' = (O, M, T, S_s, B, C, E, R') $$
     where:
    *   $R' = R \cup \{ (id, s_a, a_f) \}$
*   **Post-conditions:** Resource state created and stored in $R$, all invariants hold true.

### `AccessResource`

*   **Pre-conditions:** The resource state must exist.
*   **Input:**  $id \in String$, $a \in \mathbb{N}$ (The amount to pay),
*    **State Transition:**
        $$ S' = S $$
*   **Post-conditions:** The server gets a payment of $a_f$ lamports, all invariants hold true.

## Security Properties

1.  **Double Spending Prevention:** It is impossible to spend the same tokens more than once. This is guaranteed by the SPL Token program and Solana's transaction execution model.
2.  **Authorization:** Only the owner of the ICO is allowed to withdraw from the escrow account.
3.  **Fee Control:** The server sets access fees for the resource, and the program ensures the correct fee is paid.
4. **Token Supply Guarantee:** The program guarantees that no new tokens can be minted past the total supply amount.
5. **Rate Limiting Guarantee**: By combining per-request payments, the servers can add rate limiting mechanics and increase the costs of exploitation.

## Conclusion

This formal specification provides a rigorous description of the ContextCoin program. It enables the creation of tools for testing, verification, and improved trust in the program's correctness. Future work will involve building more complex models and proofs.