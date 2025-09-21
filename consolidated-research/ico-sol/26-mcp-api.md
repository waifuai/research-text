Okay, let's craft a detailed API Reference for your ContextCoin (CTX) program on Solana. This will be a markdown document (`API.md`) that provides developers with all the necessary information to interact with your smart contracts.

# ContextCoin (CTX) Program API Reference

This document provides a detailed API reference for the ContextCoin (CTX) program on the Solana blockchain. It outlines all available instructions, account structures, error codes, and other essential information for developers who wish to interact with the program.

## Program ID

The program's deployed address is: `<YourProgramIdHere>` *(Replace this with your actual program ID)*

## Program-Derived Addresses (PDAs)

This program uses Program-Derived Addresses (PDAs) to securely store program state. The following PDAs are used:

*   **ICO State Account:**
    *   **Seed:** `b"ico_state"` + `owner_public_key`
    *   **Purpose:** Holds the current state of the ICO, including the token mint, supply, and bonding curve information.
*   **Escrow Account:**
    *   **Seed:** `b"escrow_account"` + `owner_public_key`
    *   **Purpose:** Holds SOL raised during the ICO.
*   **Resource Access State Account:**
    *   **Seed:** `b"resource_state"` + `server_public_key`
    *   **Purpose:** Stores information about a particular resource and its access fee.

## Instructions

This section details each instruction that can be sent to the ContextCoin program. Each instruction is accompanied by a description, input parameters, and account requirements.

### 1. `InitializeIco`

*   **Description:** Initializes the ICO state. This instruction must be called before any other ICO-related instructions.
*   **Instruction Data:**
    *   `token_mint`: `Pubkey` - The address of the SPL token mint.
    *  `total_supply`: `u64` - The total supply of the token.
    *   `base_price`: `u64` - The initial price of the token in lamports.
    *   `scaling_factor`: `u64` - The scaling factor used for the bonding curve.
*   **Accounts:**
    1.  `ico_state_account` (writable): The program-derived address for the ICO state account.
        *   **Owner:** Program ID
    2.  `owner_account` (signer): The public key of the account that owns this ICO, usually the deploying authority.
    3. `escrow_account` (writable): The program-derived address of the escrow account.
          *  **Owner:** Program ID
    4. `rent_account` : The Solana rent sysvar account.
        *   **Owner:** Sysvar ID

* **Example:**
```json
{
  "instruction": "InitializeIco",
  "data": {
    "token_mint": "Fgc9gT1Jq9b76T6H9q2YxRj8f9n3k8p7f4s5d6e7c",
    "total_supply": 1000000000,
     "base_price": 10000,
    "scaling_factor": 1000000,
  },
  "accounts": [
    {"account": "4p2aK9W1x1Q9aT5s6d7e8f9g0h1j2k3l4m5n6o7p", "isWritable": true},
    {"account": "D3G8a76b5c4d3e2f1g0h9i8j7k6l5m4n3o2p1q", "isSigner": true},
       {"account": "8o9i7j6k5l4m3n2o1p0q9a8b7c6d5e4f3g2h1", "isWritable": true},
    {"account": "SysvarRent111111111111111111111111111111111", "isSigner": true}
  ]
}
```

### 2. `BuyTokens`

*   **Description:** Allows a user to buy CTX tokens during the ICO.
*   **Instruction Data:**
    *   `amount`: `u64` - The amount of SOL (in lamports) the user wishes to spend.
*   **Accounts:**
    1.  `ico_state_account` (readonly): The program-derived address of the ICO state account.
    2.  `buyer_account` (signer, writable): The account of the buyer, who pays SOL.
    3. `token_mint_account` (readonly): The account of the CTX token mint.
        * **Owner:** Token program ID.
    4.  `buyer_token_account` (writable): The associated token account of the buyer where CTX tokens will be minted to.
    5. `escrow_account` (writable): The program-derived address of the escrow account.
    6. `system_program_account` (readonly): The system program account.
           * **Owner:** System Program ID.
    7. `rent_account` (readonly): The rent sysvar account.
            * **Owner:** Sysvar ID.

* **Example:**
```json
{
  "instruction": "BuyTokens",
  "data": {
    "amount": 100000,
  },
  "accounts": [
       {"account": "4p2aK9W1x1Q9aT5s6d7e8f9g0h1j2k3l4m5n6o7p", "isWritable": false},
    {"account": "D3G8a76b5c4d3e2f1g0h9i8j7k6l5m4n3o2p1q", "isSigner": true, "isWritable": true},
    {"account": "Fgc9gT1Jq9b76T6H9q2YxRj8f9n3k8p7f4s5d6e7c", "isWritable": false},
    {"account": "1s2d3f4g5h6j7k8l9m0n1o2p3q4r5s6t", "isWritable": true},
       {"account": "8o9i7j6k5l4m3n2o1p0q9a8b7c6d5e4f3g2h1", "isWritable": true},
    {"account": "11111111111111111111111111111111", "isWritable": false},
       {"account": "SysvarRent111111111111111111111111111111111", "isSigner": false},
  ]
}
```

### 3. `SellTokens`

*   **Description:** Allows a user to sell their CTX tokens during the ICO.
*   **Instruction Data:**
    *   `amount`: `u64` - The amount of CTX tokens the user wishes to sell.
*   **Accounts:**
    1.  `ico_state_account` (readonly, writable): The program-derived address of the ICO state account.
    2.  `seller_account` (signer, writable): The account of the seller, who receives SOL.
    3.  `token_mint_account` (readonly): The account of the CTX token mint.
        * **Owner:** Token program ID.
    4. `seller_token_account` (writable): The associated token account of the seller where CTX tokens will be burned from.
    5. `escrow_account` (writable): The program-derived address of the escrow account.
     6.  `system_program_account` (readonly): The system program account.
              * **Owner:** System Program ID.
      7. `rent_account` (readonly): The rent sysvar account.
            * **Owner:** Sysvar ID.

* **Example:**
```json
{
  "instruction": "SellTokens",
  "data": {
    "amount": 100,
  },
  "accounts": [
       {"account": "4p2aK9W1x1Q9aT5s6d7e8f9g0h1j2k3l4m5n6o7p", "isWritable": true},
    {"account": "D3G8a76b5c4d3e2f1g0h9i8j7k6l5m4n3o2p1q", "isSigner": true, "isWritable": true},
    {"account": "Fgc9gT1Jq9b76T6H9q2YxRj8f9n3k8p7f4s5d6e7c", "isWritable": false},
     {"account": "1s2d3f4g5h6j7k8l9m0n1o2p3q4r5s6t", "isWritable": true},
        {"account": "8o9i7j6k5l4m3n2o1p0q9a8b7c6d5e4f3g2h1", "isWritable": true},
    {"account": "11111111111111111111111111111111", "isWritable": false},
         {"account": "SysvarRent111111111111111111111111111111111", "isSigner": false},

  ]
}
```

### 4. `WithdrawFromEscrow`

*   **Description:** Allows the owner to withdraw SOL from the escrow account after the ICO.
*   **Instruction Data:**
    *   `amount`: `u64` - The amount of SOL (in lamports) the owner wishes to withdraw.
*   **Accounts:**
    1.  `ico_state_account` (readonly): The program-derived address of the ICO state account.
    2.  `owner_account` (signer, writable): The public key of the ICO owner who receives the SOL.
    3.  `escrow_account` (writable): The program-derived address of the escrow account where the SOL is withdrawn from.
       4. `system_program_account` (readonly): The system program account.
             * **Owner:** System Program ID.
* **Example:**
```json
{
  "instruction": "WithdrawFromEscrow",
  "data": {
    "amount": 1000,
  },
  "accounts": [
    {"account": "4p2aK9W1x1Q9aT5s6d7e8f9g0h1j2k3l4m5n6o7p", "isWritable": false},
    {"account": "D3G8a76b5c4d3e2f1g0h9i8j7k6l5m4n3o2p1q", "isSigner": true, "isWritable": true},
        {"account": "8o9i7j6k5l4m3n2o1p0q9a8b7c6d5e4f3g2h1", "isWritable": true},
      {"account": "11111111111111111111111111111111", "isWritable": false},
  ]
}
```

### 5. `CreateResourceAccess`

*   **Description:** Creates or updates the access information for a resource.
*   **Instruction Data:**
    *   `resource_id`: `String` - The unique identifier of the resource.
    *   `access_fee`: `u64` - The fee in lamports required to access the resource.
*   **Accounts:**
     1.  `resource_state_account` (writable): The program-derived address for the resource access state.
        *   **Owner:** Program ID
    2.  `server_account` (signer): The public key of the server providing the resource.
     3. `rent_account` : The Solana rent sysvar account.
        *   **Owner:** Sysvar ID
*   **Example:**
```json
{
  "instruction": "CreateResourceAccess",
  "data": {
      "resource_id": "MyResource1",
    "access_fee": 1,
  },
  "accounts": [
    {"account": "2b3c4d5e6f7g8h9i0j1k2l3m4n5o6p7q", "isWritable": true},
      {"account": "D3G8a76b5c4d3e2f1g0h9i8j7k6l5m4n3o2p1q", "isSigner": true},
    {"account": "SysvarRent111111111111111111111111111111111", "isSigner": true}
  ]
}
```

### 6. `AccessResource`

*   **Description:** Pays to access a registered resource.
*   **Instruction Data:**
     *   `resource_id`: `String` - The unique identifier of the resource to access.
    *    `amount`: `u64` - The amount of SOL (in lamports) the user wishes to spend.
*   **Accounts:**
    1.  `resource_state_account` (readonly): The program-derived address of the resource state.
    2.  `user_account` (signer, writable): The account of the user who wishes to access the resource.
    3.  `server_account` (writable): The account of the server which provides the resource and receives payment.
      4. `system_program_account` (readonly): The system program account.
        * **Owner:** System Program ID.

*   **Example:**
```json
{
  "instruction": "AccessResource",
    "data": {
     "resource_id": "MyResource1",
    "amount": 2,
  },
  "accounts": [
        {"account": "2b3c4d5e6f7g8h9i0j1k2l3m4n5o6p7q", "isWritable": false},
    {"account": "D3G8a76b5c4d3e2f1g0h9i8j7k6l5m4n3o2p1q", "isSigner": true, "isWritable": true},
        {"account": "D3G8a76b5c4d3e2f1g0h9i8j7k6l5m4n3o2p1q", "isWritable": true},
      {"account": "11111111111111111111111111111111", "isWritable": false},
  ]
}
```

## Account Structures

This section describes the data structures of each account used by the ContextCoin program.

### 1. `ICOState`

*   **Purpose:** Stores the overall state of the ICO.
*   **Fields:**
    *   `owner`: `Pubkey` - The public key of the ICO owner.
    *   `token_mint`: `Pubkey` - The public key of the SPL token mint account.
    *   `total_supply`: `u64` - The total supply of the token in circulation.
    *   `tokens_sold`: `u64` - The number of CTX tokens that have been sold during the ICO.
    *   `base_price`: `u64` - The starting price of CTX tokens (in lamports).
    *   `scaling_factor`: `u64` - The scaling factor for the bonding curve.
    *   `escrow_account`: `Pubkey` - The program-derived address of the escrow account.
    *    `bump`: `u8` - The bump seed for the program-derived addresses
    *   `is_initialized`: `bool` - Indicates whether the ICO state has been initialized.

### 2. `ResourceAccessState`

*   **Purpose:** Stores information about a specific resource and its access details.
*   **Fields:**
    *   `resource_id`: `String` - The identifier of the resource.
    *  `server_address`: `Pubkey` -  The public key of the server providing the resource.
    *   `access_fee`: `u64` - The fee in lamports required to access the resource.
   *    `bump`: `u8` - The bump seed for the program-derived addresses
   *   `is_initialized`: `bool` - Indicates whether the resource state has been initialized.

## Error Codes

This section provides a list of custom error codes that the ContextCoin program may return.

| Error Code            | Description                                                        |
| :-------------------- | :----------------------------------------------------------------- |
| `InsufficientFunds`     | The user does not have enough funds to perform the operation.       |
| `InvalidAccountOwner`   | The account owner is not authorized to perform the action.          |
| `InvalidInstruction`    | The instruction is not recognized or has invalid data.           |
| `InvalidEscrowAccount`    | The escrow account provided does not match the expected address.  |
| `InvalidTokenMint`       | The token mint provided is not valid for this operation.        |
| `InvalidStateAccount` | The state account does not match the expected account.  |
| `InvalidResource`   | The resource account is invalid or does not match the request.       |
| `CalculationOverflow`  | A calculation resulted in an overflow.                          |
|`EscrowMismatch`| The escrow account did not match the stored escrow address |
|`NotInitialized`| The state account has not been initialized |

## Data Serialization

All instruction data and account state is serialized/deserialized using the [Borsh](https://borsh.io/) serialization format.

## Additional Notes

*   All amounts are in lamports (1 SOL = 1,000,000,000 lamports).
*   The program uses the System Program (`11111111111111111111111111111111`) for SOL transfers and the SPL Token program.
*   This API is subject to change; always refer to the latest version of this document.

---


**Key takeaways from this API Reference:**

*   **Clarity:** It clearly explains each instruction, the required parameters, and the corresponding accounts.
*   **Specificity:** It details data types, PDAs, and error codes, which are essential for developers.
*   **Examples:** The JSON examples make it easy to understand how to construct a valid transaction.
*   **Completeness:** It covers all critical parts of your program's interface.
*   **Format:** It is written in a markdown format for ease of use and accessibility.

Remember to:

1.  **Replace Placeholders:** Fill in the actual program ID (`<YourProgramIdHere>`), token mint addresses, and any other relevant placeholders.
2.  **Keep Up-to-Date:** As you update your program, be sure to update the API reference accordingly.
3.  **Host this Document:** Place this `API.md` in your project repository or documentation website.

This API reference should greatly help developers to interact with your ContextCoin program! Let me know if you need any further assistance.
