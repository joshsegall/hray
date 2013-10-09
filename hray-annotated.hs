-- General implementation notes:
--
-- We make heavy use of type inference to avoid having to specify types.
-- I've provided type annotations in comments to assist in understanding.
--
-- For vectors and colors (RGB triples) we use a list of three Floats
-- (i.e. [Float]). Lists allow the use of many Haskell functions and
-- abbreviations. For clarity I will use the following definitions in type
-- annotation:
--
-- type Vector = [Float]
-- type Color = [Float]

-- Bits needed for testBit
-- Also, the comment shows how to run the program
import Data.Bits    -- hray > hray.ppm

-- ByteString needed for byte-based putStr and pack
import qualified Data.ByteString as B

-- The condensed version uses these abbreviations to save space.
-- For the purposes of clarity we expand them in this version of the code
z = zipWith
k = map

-- Here are the vector operations:

-- vector addition
-- (&) :: Vector -> Vector -> Vector
(&) = zipWith (+)

-- vector scaling
-- (%) :: Vector -> Float -> Vector
v % c = z (*) v [c,c,c]

-- dot product
-- (#) :: Vector -> Vector -> Float
a # b = sum $ zipWith (*) a b

-- normalization
-- o :: Vector -> Vector
o v = v % (1 / sqrt(v # v))

-- cross product
-- (*^) :: Vector -> Vector -> Vector
[x,y,z] *^ [a,b,c] = [y*c-z*b, z*a-x*c, x*b-y*a]

-- u converts the bit-vectors into center points of the spheres
-- We iterate over the bits and create a spehere for each "on" bit
-- u :: (Float, Int) -> Vector
u (y,r) = map (\x -> [-x,0,-y-4]) [fromIntegral x | x <- [0..19], testBit r x]

-- This defines the spheres using bit vectors.
-- Note: this was the only case where type inference failed and had to be
-- explicitly annotated.
--
--          1          -- 512
--     1    1          -- 16896
--          1     111  -- 526
--     1  11111  1   1 -- 2 369
--     1    1    1     -- 16912
--     1    1     111  -- 1691 
--     1    1        1 -- 16897
-- 1   1    1    1   1 -- 279 57
--  111      11   111  -- 229774
--
b :: [Int]
b = [229774,279057,16897,16910,16912,20369,526,512,16896]

-- j stores the computed list of sphere positions
-- j :: [Vector]
j = concat $ map u $ zip [1..] b 

-- q performs ray sampling with the world
--   x = origin
--   d = direction
-- q :: Vector -> Vector -> Color
q x d = a x d $ i x d   -- calls i to calculate intersection

-- a is a helper function of q
--   x = origin
--   d = direction
--   m = type of intersection
--   t = distance to intersection
--   n = surface normal of intersection
-- a :: Vector -> Vector -> (Int, Float, Vector) -> Color
a x d (m,t,n)
  -- if m is 0, sky was hit, calculate sky color
  | m == 0 = [0.7, 0.6, 1] % ((1 - (d!!2)) ** 4)
  -- if m is 2, a sphere was hit, cast a reflected ray
  | m == 2 = [p, p, p] & ((q h r) % 0.5)
  -- otherwise it's the ground, calculate ground color
  | True =
    let z = map ceiling $ h % 0.2
        s = b * 0.2 + 0.1               -- intensity factor
    in if mod (z!!0 + z!!1) 2 > 0       -- checkerboard test
      then [3, 1, 1] % s                -- red
      else [3, 3, 3] % s                -- grey
  where
    h = x & (d % t)                     -- intersection coordinates
    l = o $ [9, 9, 16] & (h % (-1))     -- direction to light
    r = d & (n % ((n # d) * (-2)))      -- reflection
    g = l # n                           -- lambertian factor
    f(e,_,_) = e                        -- helper to extract first of 3-tuple
    b = if g < 0 || (f $ i h l) /= 0    -- illumination factor
        then 0
        else g
    p = if b > 0
        then (l # r) ** 99              -- color with specular component
        else 0

-- p is a helper function to extract the second element of a 3-tuple
-- p :: (Int, Float, Vector) -> Float
p(_,e,_) = e

-- h performs sphere intersection testing and returns a tuple of
-- (2, distance, normal), where 2 is a constant indicating a sphere
--   r = origin
--   d = direction
--   z = sphere position to test
-- h :: Vector -> Vector -> Vector -> (Int, Float, Vector)
h r d z =
  if l > 0 && s > 0.01
    then (2, s, o $ p & (d % s))    -- (2=sphere, distance, surface normal)
    else (2, 1e7,[])                -- "empty" marker (beyond the sky)
  where                             -- calculate sphere intersection
    p = r & z
    b = p # d
    l = b * b - (p # p) + 1
    s = -b - sqrt l                 -- camera-sphere distance

-- comparator for the second element of tuples
-- n :: (Int, Float, Vector) -> (Int, Float, Vector) -> (Int, Float, Vector)
n a b = if p a <= p b then a else b

-- m takes a list of intersection tuples and returns the closest one
-- m :: [(Int, Float, Vector)] -> (Int, Float, Vector)
m x = foldl1 n x

-- i is the main intersection function
--   r = origin
--   d = direction
-- i :: Vector -> Vector -> (Int, Float, Vector)
i r d =
  -- call m to find closest intersection
  m $ c : if p > 0.01               -- determines ground or sky
    then [(1, p, [0,0,1])]          -- downward, ground
    else [(0, 1e6, [])]             -- upward, sky
  where 
    p = - (r!!2) / (d!!2)
    c = m $ map (h r d) j           -- test intersection with sphere list (j)

-- e sends out multiple sample rays for a single pixel
-- e :: Vector -> Vector -> Vector -> (Float, Float) -> Color
e a b c t =
 let z = [-28,0,28]     -- jitter for the ray
 -- generate multiple rays, trace them, and accumulate result
 in (foldl1 (&) [(f a b c t ((a % v) & (b % w))) | v <- z, w<- z]) % 7

-- f traces a single ray into the world
-- f :: Vector -> Vector -> Vector -> (Float, Float) -> Color
f a b c (x,y) t =
 let r = [17, 16, 8] & t                                 -- ray origin
     d = o $ (t % (-1)) & (((a % x) & (b % y) & c) % 16) -- ray directio
 in (q r d) % 3.5                                        -- do the 

-- main function
main =
  do putStr "P6 512 512 255 "           -- write PPM header to stdout
     B.putStr $ B.pack $ map round p    -- write pixels to stdout
  where
    g = o [-6,-16,0]                    -- camera direction
    a = (o $ [0,0,1] *^ g) % 0.002      -- up vector
    b = (o $ (g *^ a)) % 0.002          -- right vector
    c = g & ((a & b) % (-256))          -- offset from eye point to focal plane
    w = [511,510..0]                    -- width/height are 512
    -- for each pixel x,y map the sampling function (e), then
    -- concatenate all the pixels components together
    p = concat $ map (e a b c) [(x,y) | y <- w, x <- w]
