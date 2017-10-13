
module Vision.DLib.DNN.Types where

import GHC.TypeLits 
import Data.Void


class HasCppType a where
  cppType :: proxy a -> String


instance KnownNat i => HasCppType i where
  cppType = show . natVal 

instance HasCppType Void where
  cppType _ = "void"


data AddTagLayer (ident :: Nat) subnet

instance (KnownNat i, HasCppType subnet) => HasCppType (AddTagLayer i subnet) where
  cppType _ = "add_tag_layer<" ++ cppType (undefined :: proxy i) ++ ", " ++ cppType (undefined :: proxy subnet) ++ ">"

type Tag1 subnet  = AddTagLayer   1 subnet
type Tag2 subnet  = AddTagLayer   2 subnet
type Tag3 subnet  = AddTagLayer   3 subnet
type Tag4 subnet  = AddTagLayer   4 subnet
type Tag5 subnet  = AddTagLayer   5 subnet
type Tag6 subnet  = AddTagLayer   6 subnet
type Tag7 subnet  = AddTagLayer   7 subnet
type Tag8 subnet  = AddTagLayer   8 subnet
type Tag9 subnet  = AddTagLayer   9 subnet
type Tag10 subnet = AddTagLayer  10 subnet



data AddSkipLayer tag subnet

instance (HasCppType tag, HasCppType subnet) => HasCppType (AddSkipLayer tag subnet) where
  cppType _ = "add_skip_layer<" ++ cppType (undefined :: proxy tag) ++ ", " ++ cppType (undefined :: proxy subnet) ++ ">"

type Skip1   subnet = AddSkipLayer (Tag1 Void)  subnet
type Skip2   subnet = AddSkipLayer (Tag2 Void)  subnet
type Skip3   subnet = AddSkipLayer (Tag3 Void)  subnet
type Skip4   subnet = AddSkipLayer (Tag4 Void)  subnet
type Skip5   subnet = AddSkipLayer (Tag5 Void)  subnet
type Skip6   subnet = AddSkipLayer (Tag6 Void)  subnet
type Skip7   subnet = AddSkipLayer (Tag7 Void)  subnet
type Skip8   subnet = AddSkipLayer (Tag8 Void)  subnet
type Skip9   subnet = AddSkipLayer (Tag9 Void)  subnet
type Skip10  subnet = AddSkipLayer (Tag10 Void) subnet



data AddPrev tag subnet

instance (HasCppType tag, HasCppType subnet) => HasCppType (AddPrev tag subnet) where
  cppType _ = "add_prev<" ++ cppType (undefined :: proxy tag) ++ ", " ++ cppType (undefined :: proxy subnet) ++ ">"

type AddPrev1   subnet = AddPrev (Tag1 Void)  subnet
type AddPrev2   subnet = AddPrev (Tag2 Void)  subnet
type AddPrev3   subnet = AddPrev (Tag3 Void)  subnet
type AddPrev4   subnet = AddPrev (Tag4 Void)  subnet
type AddPrev5   subnet = AddPrev (Tag5 Void)  subnet
type AddPrev6   subnet = AddPrev (Tag6 Void)  subnet
type AddPrev7   subnet = AddPrev (Tag7 Void)  subnet
type AddPrev8   subnet = AddPrev (Tag8 Void)  subnet
type AddPrev9   subnet = AddPrev (Tag9 Void)  subnet
type AddPrev10  subnet = AddPrev (Tag10 Void) subnet



data AvgPool (nr :: Nat) (nc :: Nat) (strideX :: Nat) (strideY :: Nat) subnet

instance (HasCppType nr, HasCppType nc, HasCppType x, HasCppType y, HasCppType subnet) => HasCppType (AvgPool nr nc x y subnet) where
  cppType _ = "avg_pool<" ++ cppType (undefined :: proxy nr) ++ ", " ++ cppType (undefined :: proxy nc) ++ ", " ++ cppType (undefined :: proxy x) ++ ", " ++ cppType (undefined :: proxy y) ++ ", " ++ cppType (undefined :: proxy subnet) ++ ">"



data Con (numFilters :: Nat) (nr :: Nat) (nc :: Nat) (strideX :: Nat) (strideY :: Nat) subnet

instance (HasCppType nf, HasCppType nr, HasCppType nc, HasCppType x, HasCppType y, HasCppType subnet) => HasCppType (Con nf nr nc x y subnet) where
  cppType _ = "con<" ++ cppType (undefined :: proxy nf) ++ ", " ++ cppType (undefined :: proxy nr) ++ ", " ++ cppType (undefined :: proxy nc) ++ ", " ++ cppType (undefined :: proxy x) ++ ", " ++ cppType (undefined :: proxy y) ++ ", " ++ cppType (undefined :: proxy subnet) ++ ">"



data Relu subnet

instance (HasCppType subnet) => HasCppType (Relu subnet) where
  cppType _ = "relu<" ++ cppType (undefined :: proxy subnet) ++ ">"



data Affine subnet 

instance (HasCppType subnet) => HasCppType (Affine subnet) where
  cppType _ = "affine<" ++ cppType (undefined :: proxy subnet) ++ ">"



data LossMetric subnet

instance (HasCppType subnet) => HasCppType (LossMetric subnet) where
  cppType _ = "loss_metric<" ++ cppType (undefined :: proxy subnet) ++ ">"



data FC (n :: Nat) subnet

instance (HasCppType n, HasCppType subnet) => HasCppType (FC n subnet) where
  cppType _ = "fc<" ++ cppType (undefined :: proxy n) ++ ", " ++ cppType (undefined :: proxy subnet) ++ ">"



data FCNoBias (n :: Nat) subnet

instance (HasCppType n, HasCppType subnet) => HasCppType (FCNoBias n subnet) where
  cppType _ = "fc_no_bias<" ++ cppType (undefined :: proxy n) ++ ", " ++ cppType (undefined :: proxy subnet) ++ ">"



data AvgPoolEverything subnet

instance (HasCppType subnet) => HasCppType (AvgPoolEverything subnet) where
  cppType _ = "avg_pool_everything<" ++ cppType (undefined :: proxy subnet) ++ ">"



data MaxPool (nr :: Nat) (nc :: Nat) (strideX :: Nat) (strideY :: Nat) subnet

instance (HasCppType nr, HasCppType nc, HasCppType x, HasCppType y, HasCppType subnet) => HasCppType (MaxPool nr nc x y subnet) where
  cppType _ = "max_pool<" ++ cppType (undefined :: proxy nr) ++ ", " ++ cppType (undefined :: proxy nc) ++ ", " ++ cppType (undefined :: proxy x) ++ ", " ++ cppType (undefined :: proxy y) ++ ", " ++ cppType (undefined :: proxy subnet) ++ ">"



data InputRGBImageSized (nr :: Nat) (nc :: Nat)

instance (HasCppType nr, HasCppType nc) => HasCppType (InputRGBImageSized nr nc) where
  cppType _ = "input_rgb_image_sized<" ++ cppType (undefined :: proxy nr) ++ ", " ++ cppType (undefined :: proxy nc) ++ ">"













