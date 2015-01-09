module Here (e,eF) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Test.MuCheck.Utils.Helpers

e :: QuasiQuoter 
e = QuasiQuoter { quoteExp = stringE . strip,
                  quotePat = undefined,
                  quoteDec = undefined,
                  quoteType = undefined }

eF :: QuasiQuoter
eF = quoteFile e

