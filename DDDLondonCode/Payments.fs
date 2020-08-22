namespace DDDLondonCode

module PaymentsPart1 =

    type CheckNumber = int64
    type CardNumber = int64

    type CardType = Visa | Mastercard
    type Currency = GBP | USD | EUR

    type PaymentType =
        | Cash
        | Check of CheckNumber
        | Card of CardType * CardNumber

    type Payment = {
        PaymentType : PaymentType
        Currency : Currency
        Amount : decimal
    }

    let chequeNumber = 987654321L
    let cardNumber = 1234567890L

    let cashPaymentGBP = { PaymentType = Cash; Currency = GBP; Amount = 100.0M }
    let chequePaymentUSD = { PaymentType = Check chequeNumber; Currency = USD; Amount = 100.0M }
    let cardPaymentEUR = { PaymentType = Card (Visa, cardNumber); Currency = EUR; Amount = 100.0M }
    let cardPaymentEUR2 = { PaymentType = Card (Visa, chequeNumber); Currency = EUR; Amount = 100.0M }

module PaymentsPart2 =

    type CheckNumber = CheckNumber of int64
    type CardNumber = CardNumber of int64

    type CardType = Visa | Mastercard
    type Currency = GBP | USD | EUR

    type PaymentType =
        | Cash
        | Check of CheckNumber
        | Card of CardType * CardNumber

    type Payment = {
        PaymentType : PaymentType
        Currency : Currency
        Amount : decimal
    }

    let chequeNumber = CheckNumber 987654321L
    let cardNumber = CardNumber 1234567890L

    let cashPaymentGBP = { PaymentType = Cash; Currency = GBP; Amount = 100.0M }
    let chequePaymentUSD = { PaymentType = Check chequeNumber; Currency = USD; Amount = 100.0M }
    let cardPaymentEUR = { PaymentType = Card (Visa, cardNumber); Currency = EUR; Amount = 100.0M }
    //let cardPaymentEUR2 = { PaymentType = Card (Visa, chequeNumber); Currency = EUR; Amount = 100.0M } // Compilation Error

module PaymentsPart3 =

    type CheckNumber = CheckNumber of int64
    type CardNumber = CardNumber of int64
    type CardType = Visa | Mastercard
    type Currency = GBP | USD | EUR
    type CardInfo = CardInfo of CardType * CardNumber

    type PaymentType =
        | Cash
        | Check of CheckNumber
        | Card of CardInfo

    type Payment = {
        PaymentType : PaymentType
        Currency : Currency
        Amount : decimal
    }

    let chequeNumber = CheckNumber 987654321L
    let cardNumber = CardNumber 1234567890L

    let cashPaymentGBP = { PaymentType = Cash; Currency = GBP; Amount = 100.0M }
    let chequePaymentUSD = { PaymentType = Check chequeNumber; Currency = USD; Amount = 100.0M }
    let cardPaymentEUR = { PaymentType = Card (CardInfo (Visa, cardNumber)); Currency = EUR; Amount = 100.0M }

module PaymentsPart4 =

    type CheckNumber = CheckNumber of int64
    type CardNumber = CardNumber of int64
    type CardType = Visa | Mastercard
    type Currency = GBP | USD | EUR
    type CardInfo = CardInfo of CardType * CardNumber

    type PaymentType =
        | Cash
        | Check of CheckNumber
        | Card of CardInfo

    type Payment = {
        PaymentType : PaymentType
        Currency : Currency
        Amount : decimal
    }

    let chequeNumber = CheckNumber 987654321L
    let cardNumber = CardNumber 1234567890L
    let cardInfo = CardInfo (Visa, cardNumber)

    let cashPaymentGBP = { PaymentType = Cash; Currency = GBP; Amount = 100.0M }
    let chequePaymentUSD = { PaymentType = Check chequeNumber; Currency = USD; Amount = 100.0M }
    let cardPaymentEUR = { PaymentType = Card cardInfo; Currency = EUR; Amount = 100.0M }

module PaymentsPart5 =

    type CheckNumber = CheckNumber of int64
    type CardNumber = CardNumber of int64
    type CardType = Visa | Mastercard
    type Currency = GBP | USD | EUR
    type CardInfo = CardInfo of CardType * CardNumber

    [<RequireQualifiedAccessAttribute>]
    module CardInfo =
        let create input =
            // TODO: validation logic -> Result<CardInfo,CardInfoValidationError>
            CardInfo input
        let value (CardInfo (cardType, cardNumber)) = 
            (cardType, cardNumber)

    type PaymentType =
        | Cash
        | Check of CheckNumber
        | Card of CardInfo

    type Payment = {
        PaymentType : PaymentType
        Currency : Currency
        Amount : decimal
    }

    let chequeNumber = CheckNumber 987654321L
    let cardNumber = CardNumber 1234567890L
    let cardInfo = CardInfo (Visa, cardNumber)

    let cashPaymentGBP = { PaymentType = Cash; Currency = GBP; Amount = 100.0M }
    let chequePaymentUSD = { PaymentType = Check chequeNumber; Currency = USD; Amount = 100.0M }
    let cardPaymentEUR = { PaymentType = Card cardInfo; Currency = EUR; Amount = 100.0M }
