# 🛒 Layaway Smart Contract

A decentralized layaway service built on Stacks blockchain that allows users to pay for items in installments and receive them after full payment completion.

## 📋 Overview

The Layaway smart contract enables:
- 🏪 Sellers to create layaway items with payment deadlines
- 💳 Buyers to make incremental payments over time
- 📦 Item claiming after full payment completion
- ❌ Automatic refunds for expired layaway agreements

## 🚀 Features

- **Create Layaway Items**: Sellers can list items with total price and payment deadline
- **Installment Payments**: Buyers can pay in multiple installments
- **Payment Tracking**: Real-time tracking of payment progress
- **Automatic Refunds**: Refunds are processed when layaway expires unpaid
- **Secure Claims**: Items can only be claimed after full payment

## 📖 Usage

### Creating a Layaway Item

```clarity
(contract-call? .layaway create-layaway-item 
  'SP1BUYER123... 
  "iPhone 15 Pro" 
  u1000000 
  u1000)
```

### Making a Payment

```clarity
(contract-call? .layaway make-payment u1 u250000)
```

### Claiming Your Item

```clarity
(contract-call? .layaway claim-item u1)
```

### Checking Item Status

```clarity
(contract-call? .layaway get-layaway-item u1)
(contract-call? .layaway get-remaining-balance u1)
(contract-call? .layaway get-payment-progress u1)
```

## 🔧 Functions

### Public Functions

| Function | Description |
|----------|-------------|
| `create-layaway-item` | Create a new layaway item |
| `make-payment` | Make an installment payment |
| `claim-item` | Claim item after full payment |
| `cancel-layaway` | Cancel expired layaway |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-layaway-item` | Get item details |
| `get-remaining-balance` | Get remaining payment amount |
| `is-fully-paid` | Check if item is fully paid |
| `get-payment-progress` | Get payment progress percentage |
| `can-claim-item` | Check if item can be claimed |

## 💡 Error Codes

- `u100`: Not authorized
- `u101`: Item not found
- `u102`: Item already exists
- `u103`: Invalid amount
- `u104`: Payment too large
- `u105`: Item not paid in full
- `u106`: Item already claimed
- `u107`: Payment deadline passed
- `u108`: Item still active

## 🛠️ Development

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Testing

```bash
clarinet test
```

### Deployment

```bash
clarinet deploy
```

## 📄 License

MIT License - feel free to use and modify as needed.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
```

