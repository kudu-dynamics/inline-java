{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Foreign.JNISpec where

import Control.Concurrent (runInBoundThread)
import Control.Exception (try)
import Data.List (sort)
import Data.Singletons
import Foreign.JNI.Internal (jniMethodToJavaSignature)
import Foreign.JNI.String (fromChars)
import Foreign.JNI.Types
import Foreign.JNI.Unsafe
import qualified Foreign.JNI.Unsafe.Internal as Internal
import Foreign.JNI.Unsafe.Internal.Introspection
import Test.Hspec

spec :: Spec
spec = do
    describe "runInAttachedThread" $ do
      it "can run jni calls in another thread" $
        runInBoundThread $ runInAttachedThread $ do
          jclass <- findClass $
            referenceTypeName (sing :: Sing ('Class "java.lang.Long"))
          deleteLocalRef jclass

      it "is needed to run jni calls in another thread" $
        runInBoundThread $ do
          findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Long")))
            `shouldThrow` \ThreadNotAttached -> True

    around_ (runInBoundThread . runInAttachedThread) $ do
      describe "isInstanceOf" $ do
        it "identifies a class name as a String" $ do
          klong <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Long")))
          name <- callObjectMethod klong classGetNameMethod []
          kstring <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.String")))
          isInstanceOf name kstring `shouldReturn` True
        it "identifies a class name as an Object" $ do
          klong <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Long")))
          name <- callObjectMethod klong classGetNameMethod []
          kobject <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Object")))
          isInstanceOf name kobject `shouldReturn` True
        it "doesn't identify a class name as a Long" $ do
          klong <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Long")))
          name <- callObjectMethod klong classGetNameMethod []
          isInstanceOf name klong `shouldReturn` False

      describe "getMethodID" $ do
        it "gives correct hints on a mistake" $ do
          kstring <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.String")))
          let sig = methodSignature [] (sing @('Class "java.lang.String"))
          result <- try $ getMethodID kstring (fromChars "replace") sig
          case result of
            Left e -> do
              sort (noSuchMethodOverloadings e) `shouldBe`
                [ "public java.lang.String java.lang.String.replace(char,char)"
                , "public java.lang.String java.lang.String.replace(java.lang.CharSequence,java.lang.CharSequence)"
                ]
            _ -> expectationFailure "call should have failed with a NoSuchMethod exception"

      describe "jniMethodToJavaSignature" $
        it "converts JNI signatures correctly" $ do
          jniMethodToJavaSignature "()J" `shouldBe` Right ([], "long")
          jniMethodToJavaSignature "(I)B" `shouldBe` Right (["int"], "byte")
          jniMethodToJavaSignature "(SF)V" `shouldBe` Right (["short", "float"], "void")
          jniMethodToJavaSignature "(C[Ljava/lang/String;D)Z"
            `shouldBe` Right (["char", "java.lang.String[]", "double"], "boolean")

      describe "showException" $
        it "correctly displays exceptions" $ do
          kinteger <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Integer")))
          let sig = methodSignature [] (sing :: Sing 'Void)
          result <- try $ Internal.getMethodID kinteger (fromChars "toString") sig
          case result of
            Left (e :: JVMException) -> do
              exStr <- showException e
              exStr `shouldSatisfy`
                (`elem`
                 [ "java.lang.NoSuchMethodError: toString\n"
                 , "java.lang.NoSuchMethodError: Ljava/lang/Integer;.toString()V\n"
                 ]
                )
            _ -> expectationFailure "call should have failed with a JVMException"
