```python
import random
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from IPython.display import display, clear_output
import time

# --- Simulation Parameters ---
EXPLORATION_RATE = 0.3  # Increased exploration
LEARNING_RATE = 0.5   # Adjusted learning rate
DISCOUNT_FACTOR = 0.95
INITIAL_BALANCE = 10000
SIMULATION_STEPS = 5000
MAX_INVENTORY = 500  # Maximum inventory allowed
INVENTORY_PENALTY_THRESHOLD = 0.8 * MAX_INVENTORY # Threshold for applying inventory penalty
INVENTORY_REWARD_THRESHOLD = 0.2 * MAX_INVENTORY # Threshold for applying inventory reward
NUM_AGENTS = 3  # Number of agents in the simulation

# --- Market Environment ---

class MarketEnvironment:
    def __init__(self, base_price=50, slope=2, price_fluctuation_range=0.05, order_book_depth=5):
        self.base_price = base_price
        self.slope = slope
        self.total_supply = 0
        self.price_history = []
        self.price_fluctuation_range = price_fluctuation_range
        self.order_book_depth = order_book_depth
        self.order_book = {"bids": [], "asks": []}  # Simplified order book
        self.transaction_history = [] # Keep a record of recent transactions
        self.event_schedule = [] # Schedule for dynamic bonding curve events
        self.current_time = 0 # Internal time tracker
        self.dynamic_event_probability = 0.05 # Probability of a dynamic event per step

        self.generate_initial_order_book()

    def generate_initial_order_book(self):
      # Generate an initial order book around the starting price
      current_price = self.get_price()
      for i in range(self.order_book_depth):
          bid_price = current_price * (1 - (i + 1) * 0.02)  # Example: Bids decrease by 2%
          ask_price = current_price * (1 + (i + 1) * 0.02)  # Example: Asks increase by 2%
          bid_amount = random.randint(10, 50)  # Random amount for each level
          ask_amount = random.randint(10, 50)
          self.order_book["bids"].append((bid_price, bid_amount))
          self.order_book["asks"].append((ask_price, ask_amount))

      # Sort bids and asks
      self.order_book["bids"].sort(key=lambda x: x[0], reverse=True)
      self.order_book["asks"].sort(key=lambda x: x[0])

    def add_dynamic_event(self, step, base_price_change, slope_change):
        self.event_schedule.append((step, base_price_change, slope_change))

    def apply_dynamic_events(self):
      if random.random() < self.dynamic_event_probability:
          # Generate a random event
          base_price_change = random.uniform(-0.1, 0.1)  # Up to +/- 10% change in base price
          slope_change = random.uniform(-0.2, 0.2)  # Up to +/- 20% change in slope
          event_step = self.current_time + random.randint(50, 200)  # Schedule the event in the future

          # Add the event to the schedule
          self.add_dynamic_event(event_step, base_price_change, slope_change)
          print(f"Scheduled dynamic event at step {event_step}: base_price_change={base_price_change:.2f}, slope_change={slope_change:.2f}")

      # Apply any scheduled events
      for event in self.event_schedule:
          if self.current_time >= event[0]:
              self.base_price *= (1 + event[1])
              self.slope *= (1 + event[2])
              self.event_schedule.remove(event)
              print(f"Applied dynamic event at step {self.current_time}: base_price={self.base_price:.2f}, slope={self.slope:.2f}")

    def get_price(self):
        # Base price with bonding curve
        price = self.base_price + self.slope * self.total_supply

        # Add random fluctuations
        fluctuation = random.uniform(
            -self.price_fluctuation_range, self.price_fluctuation_range
        )
        price *= 1 + fluctuation

        # Keep track of price history
        self.price_history.append(price)
        return price

    def get_state(self):
        # Determine price trend based on recent history
        if len(self.price_history) >= 5:
            # Calculate moving average to determine trend
            ma_current = np.mean(self.price_history[-3:])
            ma_previous = np.mean(self.price_history[-5:-2])

            if ma_current > ma_previous * 1.01: # 1% tolerance for trend
                trend = "Upward"
            elif ma_current < ma_previous * 0.99: # 1% tolerance for trend
                trend = "Downward"
            else:
                trend = "Stable"
        else:
            trend = "Stable"

        # Determine supply level
        if self.total_supply < INITIAL_BALANCE * 0.1: # 10% of initial balance as low supply
            supply_level = "Low"
        elif self.total_supply < INITIAL_BALANCE * 0.5: # 50% of initial balance as medium supply
            supply_level = "Medium"
        else:
            supply_level = "High"

        # Get best bid and ask prices from the order book
        best_bid = self.order_book["bids"][0][0] if self.order_book["bids"] else 0
        best_ask = self.order_book["asks"][0][0] if self.order_book["asks"] else float('inf')

        # Include recent transaction history
        recent_transactions = self.transaction_history[-5:]

        return {
            "trend": trend,
            "supply_level": supply_level,
            "best_bid": best_bid,
            "best_ask": best_ask,
            "recent_transactions": recent_transactions
        }

    def buy(self, amount, agent_balance):
      total_cost = 0
      remaining_amount = amount

      # Consume the order book
      asks_to_remove = []
      for i, (ask_price, ask_amount) in enumerate(self.order_book["asks"]):
          if agent_balance >= total_cost + ask_price * min(remaining_amount, ask_amount):
              if remaining_amount >= ask_amount:
                  total_cost += ask_price * ask_amount
                  remaining_amount -= ask_amount
                  asks_to_remove.append(i)
                  self.transaction_history.append((self.current_time, ask_price, ask_amount, 'buy'))
              else:
                  total_cost += ask_price * remaining_amount
                  self.order_book["asks"][i] = (ask_price, ask_amount - remaining_amount)
                  self.transaction_history.append((self.current_time, ask_price, remaining_amount, 'buy'))
                  remaining_amount = 0
                  break
          else:
              # Agent cannot afford to completely fill this order
              affordable_amount = agent_balance - total_cost // ask_price
              if affordable_amount > 0:
                  total_cost += ask_price * affordable_amount
                  self.order_book["asks"][i] = (ask_price, ask_amount - affordable_amount)
                  self.transaction_history.append((self.current_time, ask_price, affordable_amount, 'buy'))
                  remaining_amount -= affordable_amount
                  agent_balance -= total_cost # Update agent's balance
              break

      # Remove filled orders
      for index in reversed(asks_to_remove):
          del self.order_book["asks"][index]

      # Add new orders if the order book is depleted
      if not self.order_book["asks"] or len(self.order_book["asks"]) < self.order_book_depth:
          self.generate_initial_order_book() # Regenerate the order book based on the current price

      # Update total supply based on the amount actually bought
      self.total_supply += (amount - remaining_amount)

      return total_cost, amount - remaining_amount

    def sell(self, amount, agent_inventory):
        total_revenue = 0
        remaining_amount = amount

        # Consume the order book
        bids_to_remove = []
        for i, (bid_price, bid_amount) in enumerate(self.order_book["bids"]):
            if remaining_amount >= bid_amount:
                total_revenue += bid_price * bid_amount
                remaining_amount -= bid_amount
                bids_to_remove.append(i)
                self.transaction_history.append((self.current_time, bid_price, bid_amount, 'sell'))
            else:
                total_revenue += bid_price * remaining_amount
                self.order_book["bids"][i] = (bid_price, bid_amount - remaining_amount)
                self.transaction_history.append((self.current_time, bid_price, remaining_amount, 'sell'))
                remaining_amount = 0
                break

        # Remove filled orders
        for index in reversed(bids_to_remove):
            del self.order_book["bids"][index]
        
        # Add new orders if the order book is depleted
        if not self.order_book["bids"] or len(self.order_book["bids"]) < self.order_book_depth:
            self.generate_initial_order_book()

        # Update total supply based on the amount actually sold
        self.total_supply -= (amount - remaining_amount)

        return total_revenue, amount - remaining_amount

    def update_time(self):
      self.current_time += 1

# --- Agent ---

class Agent:
    def __init__(self, agent_id):
        self.agent_id = agent_id
        self.q_table = {}  # Q-table (state_tuple, action_tuple) -> value
        self.balance = INITIAL_BALANCE
        self.inventory = 0
        self.last_action = None
        self.last_market_state = None
        self.transaction_history = []

    def choose_action(self, state, valid_actions):
        if random.random() < EXPLORATION_RATE:
            # Explore
            action_type = random.choice(valid_actions)
            if action_type == "buy":
                amount = random.randint(1, min(50, int(self.balance / state["best_ask"]))) # Buy up to 50, or whatever the balance allows
            elif action_type == "sell":
                amount = random.randint(1, min(50, self.inventory)) # Sell up to 50 or whatever the inventory has
            else:
                amount = 0 # Hold
            return (action_type, amount)
        else:
            # Exploit
            state_tuple = self.get_state_tuple(state)
            q_values = {}

            for action_type in valid_actions:
              if action_type == "buy":
                for amount in range(1, min(50, int(self.balance / state["best_ask"])) + 1):
                  action_tuple = (action_type, amount)
                  q_values[action_tuple] = self.q_table.get(state_tuple, {}).get(action_tuple, 0.0)
              elif action_type == "sell":
                for amount in range(1, min(50, self.inventory) + 1):
                  action_tuple = (action_type, amount)
                  q_values[action_tuple] = self.q_table.get(state_tuple, {}).get(action_tuple, 0.0)
              else:
                action_tuple = (action_type, 0)
                q_values[action_tuple] = self.q_table.get(state_tuple, {}).get(action_tuple, 0.0)

            # Find the action with the highest Q-value
            best_action = max(q_values, key=q_values.get)
            return best_action

    def get_state_tuple(self, state):
      # Convert the state dictionary into a hashable tuple
      # Flatten the recent transactions into a tuple
      recent_transactions_flat = tuple(item for sublist in state["recent_transactions"] for item in sublist)

      return (
          state["trend"],
          state["supply_level"],
          state["best_bid"],
          state["best_ask"],
          recent_transactions_flat
      )

    def get_valid_actions(self):
      valid_actions = ["hold"]
      if self.balance > 0:
          valid_actions.append("buy")
      if self.inventory > 0:
          valid_actions.append("sell")
      return valid_actions

    def update_q_table(self, current_state, action, reward, new_state):
        current_state_tuple = self.get_state_tuple(current_state)
        new_state_tuple = self.get_state_tuple(new_state)
        action_tuple = action

        if current_state_tuple not in self.q_table:
            self.q_table[current_state_tuple] = {}
        if action_tuple not in self.q_table[current_state_tuple]:
            self.q_table[current_state_tuple][action_tuple] = 0.0

        if new_state_tuple not in self.q_table:
          self.q_table[new_state_tuple] = {}
          # Initialize possible actions in the new state
          for next_action_type in ["buy", "sell", "hold"]:
            if next_action_type == "buy":
              for amount in range(1, 51):
                self.q_table[new_state_tuple][(next_action_type, amount)] = 0.0
            elif next_action_type == "sell":
              for amount in range(1, 51):
                self.q_table[new_state_tuple][(next_action_type, amount)] = 0.0
            else:
              self.q_table[new_state_tuple][(next_action_type, 0)] = 0.0

        current_q = self.q_table[current_state_tuple][action_tuple]

        # Find the maximum Q-value for the next state among all possible actions
        max_next_q = max(self.q_table[new_state_tuple].values(), default=0.0)

        new_q = (1 - LEARNING_RATE) * current_q + LEARNING_RATE * (
            reward + DISCOUNT_FACTOR * max_next_q
        )
        self.q_table[current_state_tuple][action_tuple] = new_q

    def calculate_reward(self, action_type, amount, cost=0, revenue=0):
        reward = 0

        if action_type == "buy":
            if cost > 0:
                reward -= cost  # Penalty for buying (spending money)
            else:
                reward = -50  # Penalty if buying wasn't possible (e.g., not enough balance)
        elif action_type == "sell":
            if revenue > 0:
                reward += revenue  # Reward for selling (getting money)
            else:
                reward = -50 # Penalty if selling wasn't possible (e.g., not enough inventory)
        elif action_type == "hold":
            # Small reward/penalty for holding to encourage some activity
            reward = 0

        # Inventory Penalty/Reward
        if self.inventory > INVENTORY_PENALTY_THRESHOLD:
          reward -= (self.inventory - INVENTORY_PENALTY_THRESHOLD) * 0.1 # Penalty for exceeding the inventory threshold
        elif self.inventory < INVENTORY_REWARD_THRESHOLD:
          reward += (INVENTORY_REWARD_THRESHOLD - self.inventory) * 0.05 # Reward for having low inventory

        return reward

# --- Simulation Loop ---

def run_simulation(agents, environment, num_steps=SIMULATION_STEPS):
    results = {agent.agent_id: [] for agent in agents}
    for step in range(num_steps):
        environment.update_time() # Update the internal time of the environment
        environment.apply_dynamic_events() # Apply scheduled dynamic events
        
        for agent in agents:
            state = environment.get_state()
            valid_actions = agent.get_valid_actions()
            action_tuple = agent.choose_action(state, valid_actions)
            action_type, amount = action_tuple

            reward = 0
            cost = 0
            revenue = 0
            actual_amount_bought = 0
            actual_amount_sold = 0

            if action_type == "buy":
                cost, actual_amount_bought = environment.buy(amount, agent.balance)
                if cost > 0:
                    agent.balance -= cost
                    agent.inventory += actual_amount_bought
                    reward = agent.calculate_reward(action_type, actual_amount_bought, cost=cost)
                else:
                    reward = agent.calculate_reward(action_type, amount, cost=0) # Not enough balance or order book issues

            elif action_type == "sell":
                revenue, actual_amount_sold = environment.sell(amount, agent.inventory)
                if revenue > 0:
                    agent.balance += revenue
                    agent.inventory -= actual_amount_sold
                    reward = agent.calculate_reward(action_type, actual_amount_sold, revenue=revenue)
                else:
                    reward = agent.calculate_reward(action_type, amount, revenue=0) # Not enough inventory or order book issues

            elif action_type == "hold":
                reward = agent.calculate_reward(action_type, amount) # No cost/revenue for hold

            new_state = environment.get_state()
            agent.update_q_table(state, action_tuple, reward, new_state)

            # Store results for analysis
            results[agent.agent_id].append(
                {
                    "step": step,
                    "agent_id": agent.agent_id,
                    "state": state,
                    "action_type": action_type,
                    "amount_requested": amount,
                    "amount_executed": amount - (amount - actual_amount_sold if action_type == "sell" else amount - actual_amount_bought),
                    "reward": reward,
                    "balance": agent.balance,
                    "inventory": agent.inventory,
                    "price": environment.get_price(),
                    "total_supply": environment.total_supply,
                    "base_price": environment.base_price,
                    "slope": environment.slope
                }
            )

            # Update last state and action
            agent.last_action = action_tuple
            agent.last_market_state = state

        # Print current status every 50 steps
        if (step + 1) % 50 == 0:
            clear_output(wait=True)
            df_list = [pd.DataFrame(results[agent_id]).tail(50) for agent_id in results]
            for df_result in df_list:
              display(df_result)
            
            # Print combined status for all agents
            print(f"Step: {step+1}")
            for agent in agents:
                print(f"  Agent {agent.agent_id}: Balance: {agent.balance:.2f}, Inventory: {agent.inventory}, Price: {environment.get_price():.2f}, Total Supply: {environment.total_supply}")
            time.sleep(0.1)

    return pd.concat([pd.DataFrame(results[agent_id]) for agent_id in results])

# --- Run the Simulation ---

# Create agents and environment
agents = [Agent(agent_id=f"agent_{i+1}") for i in range(NUM_AGENTS)]
environment = MarketEnvironment()

# Run the simulation
df_results = run_simulation(agents, environment, num_steps=SIMULATION_STEPS)

# --- Analysis and Visualization ---
# (Modify the plotting functions to handle multiple agents)

# 1. Plot Balance and Inventory for each agent
plt.figure(figsize=(12, 6))
for agent_id in df_results["agent_id"].unique():
    agent_data = df_results[df_results["agent_id"] == agent_id]
    plt.plot(agent_data["step"], agent_data["balance"], label=f"{agent_id} Balance")
    plt.plot(agent_data["step"], agent_data["inventory"], label=f"{agent_id} Inventory")

plt.xlabel("Step")
plt.ylabel("Value")
plt.title("Agents' Balance and Inventory Over Time")
plt.legend()
plt.show()

# 2. Plot Market Price
plt.figure(figsize=(12, 6))
plt.plot(df_results["step"].unique(), df_results["price"].iloc[:len(df_results["step"].unique())], label="Price")
plt.xlabel("Step")
plt.ylabel("Price")
plt.title("Market Price Over Time")
plt.legend()
plt.show()

# 3. Plot Actions for each agent
plt.figure(figsize=(12, 6))
for agent_id in df_results["agent_id"].unique():
    agent_data = df_results[df_results["agent_id"] == agent_id]
    plt.scatter(
        agent_data[agent_data["action_type"] == "buy"]["step"],
        agent_data[agent_data["action_type"] == "buy"]["amount_executed"],
        label=f"{agent_id} Buy",
        alpha=0.5,
    )
    plt.scatter(
        agent_data[agent_data["action_type"] == "sell"]["step"],
        agent_data[agent_data["action_type"] == "sell"]["amount_executed"],
        label=f"{agent_id} Sell",
        alpha=0.5,
    )

plt.xlabel("Step")
plt.ylabel("Amount")
plt.title("Agents' Buy/Sell Actions Over Time")
plt.legend()
plt.show()

# 4. Plot Rewards for each agent
plt.figure(figsize=(12, 6))
for agent_id in df_results["agent_id"].unique():
    agent_data = df_results[df_results["agent_id"] == agent_id]
    plt.plot(agent_data["step"], agent_data["reward"], label=f"{agent_id} Reward")

plt.xlabel("Step")
plt.ylabel("Reward")
plt.title("Agents' Reward Over Time")
plt.legend()
plt.show()

# 5. Plot Base Price and Slope (Dynamic Bonding Curve)
plt.figure(figsize=(12, 6))
plt.plot(df_results["step"].unique(), df_results["base_price"].iloc[:len(df_results["step"].unique())], label="Base Price", color="blue")
plt.plot(df_results["step"].unique(), df_results["slope"].iloc[:len(df_results["step"].unique())], label="Slope", color="orange")
plt.xlabel("Step")
plt.ylabel("Value")
plt.title("Dynamic Bonding Curve Parameters Over Time")
plt.legend()
plt.show()

# 6. Heatmap of Q-table (for a specific action, e.g., "buy") and a specific agent
def plot_q_table_heatmap(agent, action_type, amount):
    q_subset = {}
    for state_tuple, actions in agent.q_table.items():
        if (action_type, amount) in actions:
            # Extract relevant state information (e.g., trend and supply level)
            simplified_state = (state_tuple[0], state_tuple[1])  # Trend, Supply Level
            q_subset[simplified_state] = actions[(action_type, amount)]

    if not q_subset:
      print(f"No Q-values found for action '{action_type}' with amount {amount}.")
      return

    q_df = pd.DataFrame.from_dict(q_subset, orient="index")
    q_df.index = pd.MultiIndex.from_tuples(q_df.index, names=["Trend", "Supply Level"])
    q_df.columns = [f"{action_type} ({amount})"]

    plt.figure(figsize=(8, 6))
    sns.heatmap(q_df, annot=True, cmap="coolwarm", cbar=True)
    plt.title(f"Q-table Heatmap for {action_type} (Amount: {amount}) - Agent {agent.agent_id}")
    plt.show()

# Example: Plot Q-values for "buy" action with amount 10 for a specific agent
plot_q_table_heatmap(agents[0], "buy", 10)  # Change the index to view different agents

# --- Performance Metrics ---

# Calculate and print metrics for each agent
for agent in agents:
    agent_data = df_results[df_results["agent_id"] == agent.agent_id]
    
    # Calculate total return
    total_return = (agent_data["balance"].iloc[-1] - INITIAL_BALANCE) / INITIAL_BALANCE
    print(f"Agent {agent.agent_id} - Total Return: {total_return * 100:.2f}%")

    # Calculate Sharpe Ratio (simplified, assuming risk-free rate = 0)
    daily_returns = agent_data["balance"].pct_change()
    sharpe_ratio = daily_returns.mean() / daily_returns.std() * np.sqrt(SIMULATION_STEPS)
    print(f"Agent {agent.agent_id} - Sharpe Ratio (annualized): {sharpe_ratio:.2f}")

    # Calculate Maximum Drawdown
    cumulative_returns = (1 + daily_returns).cumprod()
    peak = cumulative_returns.expanding(min_periods=1).max()
    drawdown = (cumulative_returns - peak) / peak
    max_drawdown = drawdown.min()
    print(f"Agent {agent.agent_id} - Maximum Drawdown: {max_drawdown * 100:.2f}%")
    print("-" * 30)
    
# Display the final Q-table for each agent
for agent in agents:
    print(f"\nFinal Q-Table for Agent {agent.agent_id}:")
    q_table_df = pd.DataFrame.from_dict(agent.q_table, orient='index')
    display(q_table_df)
```

**Changes Made:**

1. **Multiple Agents:**
    *   Introduced a `NUM_AGENTS` parameter to control the number of agents.
    *   Created a list of `Agent` objects in the `agents` variable.
    *   Modified the `run_simulation` function to loop through each agent in each step.
    *   The `results` dictionary now stores data for each agent separately, keyed by their `agent_id`.
    *   The output and plots are now generated for each agent individually.

2. **Plotting and Analysis:**
    *   Updated the plotting functions to handle multiple agents. The plots now show lines or scatter points for each agent, with different colors and labels.
    *   The performance metrics (total return, Sharpe ratio, maximum drawdown) are calculated and printed for each agent separately.

3. **Display Q-table:**
    *   The final Q-table is displayed for each agent.

**How to Run and Interpret:**

1. **Run the code:** Copy and paste the code into a Python environment (like a Jupyter notebook or a Python script) and run it.
2. **Observe the output:** You'll see the simulation progress, with the balance, inventory, and actions of each agent printed every 50 steps.
3. **Analyze the plots:**
    *   The **Balance and Inventory** plot shows how each agent's balance and inventory change over time.
    *   The **Market Price** plot remains the same, showing the overall market price.
    *   The **Actions** plot now shows the buy/sell actions of each agent, allowing you to compare their strategies.
    *   The **Rewards** plot shows the rewards received by each agent.
    *   The **Q-table Heatmap** can be used to visualize the Q-table of a specific agent for a particular action and amount.
4. **Examine the performance metrics:** The total return, Sharpe ratio, and maximum drawdown will be printed for each agent, giving you a quantitative measure of their performance.
5. **Experiment:** Change the parameters, especially `NUM_AGENTS`, `EXPLORATION_RATE`, and the parameters related to the market environment (e.g., `price_fluctuation_range`, `order_book_depth`). See how the agents' behavior and performance change.

**Further Development (Advanced RL):**

*   **Deep Q-Networks (DQN):** Replace the Q-table with a neural network to approximate the Q-function. This allows the agent to handle more complex state spaces. You can use libraries like TensorFlow or PyTorch to implement the neural network.
*   **Double DQN:** An improvement over DQN that reduces overestimation bias.
*   **Dueling DQN:** Another improvement that separates the estimation of state value and action advantages.
*   **Prioritized Experience Replay:** Instead of sampling experience uniformly from the replay buffer, prioritize experiences based on their TD error.
*   **Policy Gradient Methods:** Explore policy gradient methods like REINFORCE, A2C, or PPO, which directly learn a policy (a mapping from states to actions) instead of a value function.
*   **Actor-Critic Methods:** Combine value-based and policy-based methods.

**Example: Implementing DQN (Conceptual)**

Here's a very basic conceptual example of how you might start to implement a DQN agent using TensorFlow/Keras:

```python
import tensorflow as tf
from tensorflow.keras import layers, models

# ... (other parts of the code remain the same)

class DQNAgent:
    def __init__(self, agent_id, state_size, action_size):
        self.agent_id = agent_id
        self.state_size = state_size
        self.action_size = action_size
        self.memory = []  # Replay buffer
        self.gamma = DISCOUNT_FACTOR    # Discount factor
        self.epsilon = EXPLORATION_RATE  # Exploration rate
        self.epsilon_min = 0.01
        self.epsilon_decay = 0.995
        self.learning_rate = LEARNING_RATE
        self.model = self._build_model()

    def _build_model(self):
        # Neural Net for Deep-Q learning Model
        model = models.Sequential()
        model.add(layers.Dense(24, input_dim=self.state_size, activation='relu'))
        model.add(layers.Dense(24, activation='relu'))
        model.add(layers.Dense(self.action_size, activation='linear'))
        model.compile(loss='mse', optimizer=tf.keras.optimizers.Adam(learning_rate=self.learning_rate))
        return model

    def choose_action(self, state, valid_actions):
        if np.random.rand() <= self.epsilon:
            # Explore
            # ... (same exploration logic as before)
        else:
            # Exploit
            act_values = self.model.predict(state) # Get Q-values from the network
            # ... (select action based on Q-values and valid_actions)

    def remember(self, state, action, reward, next_state, done):
        self.memory.append((state, action, reward, next_state, done))

    def replay(self, batch_size):
        minibatch = random.sample(self.memory, batch_size)
        for state, action, reward, next_state, done in minibatch:
            target = reward
            if not done:
                target = (reward + self.gamma * np.amax(self.model.predict(next_state)[0]))
            target_f = self.model.predict(state)
            target_f[0][action] = target  # Update Q-value for the taken action
            self.model.fit(state, target_f, epochs=1, verbose=0)
        if self.epsilon > self.epsilon_min:
            self.epsilon *= self.epsilon_decay

# ... (rest of the simulation loop)

# In the main loop:
# - Create DQNAgent objects instead of Agent objects
# - Call agent.remember() to store experiences
# - Call agent.replay() periodically to train the network
```

This is a very simplified example, and a full DQN implementation would require more careful handling of the state representation, action space, network architecture, and training process. There are also many online resources and tutorials available for implementing DQN and other advanced RL algorithms. Remember to install `tensorflow` or `torch` libraries. Please let me know if you have any other questions.
