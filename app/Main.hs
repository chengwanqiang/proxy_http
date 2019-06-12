module Main where

import Network.Socket
import Network.Socket.ByteString as BS
import Foreign
import Foreign.C
import Network.Socket.Internal
import Control.Monad
import qualified Data.ByteString as DBS
import Control.Concurrent
import GHC.IO.Handle
import System.Posix.IO
import System.Posix.Types
import System.Environment
import Text.Regex.XMLSchema.String as TR
import Data.String.Conversions.Monomorphic as DS

main :: IO ()
main = do
    cmdArgs <- getArgs
    let cPort = head cmdArgs
    let sAddr = cmdArgs !! 1
    let sPort = cmdArgs !! 2
    --运行prox
    runProxy cPort (mySplit sAddr) sPort

mySplit :: String -> [String]
mySplit sAddr = do
        TR.tokenize "[0-9]*" sAddr -- ["","","",""]
        -- map (read :: Word8) sAddrL


runProxy :: String -> [String] -> String -> IO ()

runProxy cPort sAddr sPort = do
    let proxyHints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
    proxyAI:_ <- getAddrInfo (Just proxyHints) Nothing (Just cPort)
    proxySock <- socket (addrFamily proxyAI) (addrSocketType proxyAI) (addrProtocol proxyAI)    
    bind proxySock (addrAddress proxyAI)
    listen proxySock 1
    forever $ do
        (clientSock,_) <- accept proxySock
        proxySocket clientSock sAddr sPort

proxySocket :: Socket -> [String] -> String -> IO ()

proxySocket clientSock sAddr sPort  = do
    --(206,189,179,153) :: 美国的codinggame服务器
    --(115,239,210,27) :: 百度的服务器
    let serverAddr = SockAddrInet (read sPort) $ tupleToHostAddress ((read (head sAddr)::Word8),(read (sAddr !! 1)::Word8),(read (sAddr !! 2)::Word8),(read (sAddr !! 3)::Word8))
    let serverAI = defaultHints { addrSocketType = Stream, addrAddress = serverAddr, addrFamily = getSockAddrFamily serverAddr  }
    serverSock <- socket (addrFamily serverAI) (addrSocketType serverAI) (addrProtocol serverAI)
    connect serverSock serverAddr

    let closeSocks = \_ -> close clientSock >> close serverSock
    forkFinally (forwardData clientSock serverSock 1) closeSocks --为了交换客户端和服务器的数据
    forkFinally (forwardData serverSock clientSock 2) closeSocks
    return ()


getSockAddrFamily :: SockAddr -> Family

getSockAddrFamily (SockAddrUnix _) = AF_UNIX

getSockAddrFamily (SockAddrInet _ _) = AF_INET

getSockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6

forwardData :: Socket -> Socket  -> Int -> IO ()

forwardData srcSock beseSock flag = do
  msg <- recv srcSock 1024 --recv :: 从套接字接收数据。
  unless (DBS.null msg) $ do -- unless :: 取反。此处表示当msg不为空时执行do语句
    case flag of 
        1 -> do
                putStrLn "-----------------------------------------------------------------------------------"
                putStrLn "客户端到服务端"
                print msg
                putStrLn "-----------------------------------------------------------------------------------"
        2 -> do
                putStrLn "-----------------------------------------------------------------------------------"
                putStrLn "服务端到客户端"
                print msg
                putStrLn "-----------------------------------------------------------------------------------"
    sendAll beseSock msg
    forwardData srcSock beseSock flag
