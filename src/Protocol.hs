module Protocol where

import Crypto.PubKey.ECC.Types(getCurveByName,CurveName(SEC_p256r1))
import Crypto.PubKey.ECC.Generate(generate)
import Crypto.PubKey.ECC.ECDSA(PublicKey(public_q), PrivateKey(private_d))
import Crypto.Random.Types(MonadRandom)
import Crypto.PubKey.ECC.Prim(pointMul,pointBaseMul, pointAdd, isPointAtInfinity )

-- Curve information  
curve = getCurveByName SEC_p256r1


data ResultVal = Equal | NotEqual | Error
  deriving (Show,Eq)

-- Alice private x, Bob private y

-- Follows conventions of variable names in OTR specificiation: https://otr.cypherpunks.ca/Protocol-v3-4.0.0.html
-- Curve points suffixed with Pt annotation and privateKey integers with Int annotation.

protocol :: MonadRandom m => (Integer,Integer) -> m ResultVal
protocol = \(x,y)->
          do                                                           --stage 1
           (g2a, a2) <- generate curve
           (g3a, a3) <- generate curve
           let (a2Int, a3Int) = (private_d a2, private_d a3)
           let (g2aPt,g3aPt) =  (public_q g2a, public_q g3a)
           (g2b, b2) <- generate curve                                 --stage 2
           let (g2bPt, b2Int) = (public_q g2b, private_d b2)
           (g3b, b3) <- generate curve
           let (g3bPt, b3Int) = (public_q g3b, private_d b3)
           let g2Pt = pointMul curve b2Int g2aPt
           let g3Pt = pointMul curve b3Int g3aPt
           (_, r) <- generate curve
           let rInt = private_d r
           let pbPt = pointMul curve rInt g3Pt
           let qbPt = pointAdd curve (pointBaseMul curve rInt) (pointMul curve y g2Pt)
           (_,s) <- generate curve                                      --stage 3
           let sInt = private_d s
           let paPt = pointMul curve sInt g3Pt
           let qaPt = pointAdd curve (pointBaseMul curve sInt) (pointMul curve x g2Pt)
           let raPt = pointAdd  curve (pointMul curve a3Int qaPt) (pointMul curve (negate a3Int) qbPt)
           let rbPt = pointAdd curve (pointMul curve b3Int qaPt) (pointMul curve (negate b3Int) qbPt)          --stage 4
           let rabPt = pointMul curve b3Int raPt
           let compValPt = pointAdd curve pbPt (pointMul curve (-1) paPt)
           let comparePt = pointAdd curve rabPt compValPt
           if isPointAtInfinity comparePt then
               return Equal
            else
                return NotEqual
           