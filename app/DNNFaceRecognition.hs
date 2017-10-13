{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


import Vision.DLib.Monad
import Vision.DLib.DNN.Types
import GHC.TypeLits
import Data.Void

-- Types needed for this module
type Residual block (n :: Nat) (bn :: * -> *) subnet = AddPrev1 (block n bn 1 (Tag1 subnet))
type ResidualDown block (n :: Nat) (bn :: * -> *) subnet = AddPrev2 (AvgPool 2 2 2 2 (Skip1 (Tag2 (block n bn 2 (Tag1 subnet)))))

type Block (n :: Nat) bn (stride :: Nat) subnet = bn (Con n 3 3 1 1 (Relu (bn (Con n 3 3 stride stride subnet))))

type ARes (n :: Nat) subnet = Relu (AddPrev1 (Block n Affine 1 (Tag1 subnet)))
type AResDown (n :: Nat) subnet = Relu (AddPrev2 (AvgPool 2 2 2 2 (Skip1 (Tag2 (Block n Affine 2 (Tag1 subnet))))))

type ALevel0 subnet = AResDown 256 subnet
type ALevel1 subnet = ARes 256 (ARes 256 (AResDown 256 subnet))
type ALevel2 subnet = ARes 128 (ARes 128 (AResDown 128 subnet))
type ALevel3 subnet = ARes 64 (ARes 64 (ARes 64 (AResDown 64 subnet)))
type ALevel4 subnet = ARes 32 (ARes 32 (AResDown 32 subnet))

type AllLevels subnet = ALevel0 (ALevel1 (ALevel2 (ALevel3 (ALevel4 subnet))))

type ANetLevels = LossMetric (FCNoBias 128 (AvgPoolEverything (AllLevels (MaxPool 3 3 2 2 (Relu (Affine (Con 32 7 7 2 2 (InputRGBImageSized 150 150))))))))


shapeFile = ""

main = do
  putStrLn $ cppType (undefined :: proxy ANetLevels)
