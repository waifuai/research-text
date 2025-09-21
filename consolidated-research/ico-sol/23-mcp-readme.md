# ContextCoin (CTX): Enabling Secure and Scalable Resource Access with MCP on Solana

ContextCoin (CTX) is a Solana-based token designed to provide a secure and scalable solution for accessing resources within the Model Context Protocol (MCP) ecosystem. This token facilitates micro-payments for resource access, effectively preventing spam and DDoS attacks, while encouraging a thriving, decentralized economy.

## Overview

This project introduces a novel way to use blockchain technology to manage access to resources within the MCP ecosystem. By requiring nominal payments for each resource access request using CTX tokens, we create a cost barrier that discourages malicious activities and aligns incentives for both resource providers and consumers.

**Key Features:**

*   **Secure Resource Access:** Uses CTX payments to prevent spam and DDoS attacks.
*   **Dynamic Pricing:** Allows resource providers to set prices based on demand.
*   **Micro-payments:** Enables efficient and cost-effective access to resources.
*   **Decentralized:** Leverages the speed and security of the Solana blockchain.
*   **Open-Source:**  Encourages community contributions and transparency.
*   **Bonding Curve ICO:** Utilizes a bonding curve model for initial token distribution.
*    **SPL Token:** Uses the standard SPL token program.
*   **Micro-Transactions:** Allows the usage of micro-transactions for resource access.
*   **Customizable:** Uses custom program-derived addresses for ease of use.

## Tokenomics

*   **Name:** ContextCoin
*   **Symbol:** CTX
*   **Total Supply:** 1,000,000,000 CTX (Fixed)
*   **Decimals:** 9
*   **Initial Distribution:**
    *   20% - Team & Development (Locked for 1 year, then vesting)
    *   30% - Ecosystem Fund (Grants, partnerships, future development)
    *   50% - Initial Sale (Bonding curve ICO)
*   **Utility:**
    *   Pay-per-access to resources within the Model Context Protocol (MCP) ecosystem.
    *   Future options for staking and governance.

## Getting Started

### Prerequisites

*   [Rust](https://www.rust-lang.org/) (latest stable)
*   [Solana Tool Suite](https://docs.solana.com/cli/install-solana-cli-tools) (v1.17 or later)
*   Basic understanding of Solana and smart contracts

### Building the Smart Contract

1.  Clone the repository:

    ```bash
    git clone <repository-url>
    cd contextcoin
    ```

2.  Build the smart contract:

    ```bash
    cargo build-bpf --target bpfel-unknown-unknown --release
    ```

### Deploying the Smart Contract

1.  Generate a new keypair for your program:

    ```bash
    solana-keygen new -o ./program-keypair.json
    ```
2.  Deploy the smart contract:

    ```bash
   solana program deploy target/bpfel-unknown-unknown/release/contextcoin.so --keypair ./program-keypair.json
    ```

    **Note:** Replace `<your-program-id>` with the output of the `solana program deploy` command.

### Running the Basic Client

1.  Update the `src/client.rs` with your program id:
    ```rust
    let program_id = Pubkey::from_str("YourProgramIdHere").expect("Invalid Program ID"); // Replace with your program ID
    ```

2.  Run the client:

    ```bash
    cargo run --package contextcoin --bin client
    ```

   This client will showcase the basic functionality of the program, allowing you to initialize the ICO, simulate buying and selling, withdraw from the escrow, and create / access resources.

## Smart Contract Details

The smart contract is written in Rust and leverages the Solana program library. It provides the following functionality:

*   **ICO Initialization:** Sets up the initial state for token distribution and pricing.

*   **Token Purchasing:** Allows users to buy CTX tokens using SOL, while adhering to the configured bonding curve.

*   **Token Selling:** Allows users to sell back CTX tokens, receiving SOL.

*  **Withdraw Funds:** Allows the owner of the ICO to withdraw the accumulated SOL from the escrow.

*  **Resource Creation:** Enables MCP resource providers to register their resources and set access fees.

*  **Resource Access:** Facilitates pay-per-access to resources, rewarding resource providers and preventing malicious attacks.

## Code Structure

*   `src/lib.rs`:  Main smart contract entry point.

*   `src/state.rs`: Data structures (ICO state, resource state).

*   `src/error.rs`: Custom error types.

*   `src/instruction.rs`: Instruction data formats.

*   `src/processor.rs`: Instruction processing logic.

*   `src/client.rs`: Basic client implementation showcasing how to interact with the smart contracts.

## How it Works

1.  **Initial Token Offering (ICO):** Tokens are sold using a linear bonding curve model.
2.  **Resource Registration:** Resource providers register their resources and set access fees on the blockchain.
3.  **Resource Access:**
    *   Users pay a small CTX fee to access a resource.
    *   Smart contracts verify payment before granting access.

## Contributing

We welcome contributions to this project! If you have ideas, bug fixes, or feature requests, please submit a pull request or open an issue on GitHub.

## Future Development

*   Staking and governance mechanisms for CTX holders.
*   Tiered access levels for resources based on CTX holdings.
*   Integration with decentralized exchanges (DEXs) on Solana.
*   More sophisticated bonding curve options.
*   Dynamic fee adjustments based on network conditions.
*   Enhanced security measures and audits.
*   CLI tooling to aid users in building on top of the contracts.

## Disclaimer

This project is provided as-is and is for educational and experimental purposes. Please use it responsibly and do your own due diligence before making any financial decisions. No guarantees are made about the security or stability of the system.

## License

This project is licensed under the MIT-0 License - see the [LICENSE](../LICENSE) file for details.