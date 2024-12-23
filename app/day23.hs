import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Graph as Graph

main :: IO()
main = do
    ls <- readLines "app/day23.testinput"
    let conns = map nodesFromLine ls
    let m = makeConnMap conns Map.empty
    let edges = Map.elems (Map.mapWithKey allEdges m)
    let (graph,nodeFromVertex,vertexFromKey) = Graph.graphFromEdges edges
    print (Graph.dfs graph [0])

-- This is really looking for all cycles with length 3 in the graph(s) of all connections

readLines :: FilePath -> IO [String]
readLines fp = lines <$> readFile fp 

nodesFromLine :: String -> (String,String)
nodesFromLine l = (take 2 l, drop 3 l)

makeConnMap :: [(String,String)] -> Map String [String] -> Map String [String]
makeConnMap [] m = m
makeConnMap cs m = makeConnMap (tail cs) (Map.insertWith (++) (fst (head cs)) [snd (head cs)] m) -- need reverse connections?

allEdges :: String -> [String] -> (String, String, [String])
allEdges k v = (k, k, v)