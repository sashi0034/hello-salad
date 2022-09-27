-- 参考: https://qiita.com/lotz/items/eb73e62a64bc208c2dd6 

module Game where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-------------------
-- Display の設定
-------------------

windowWidth, windowHeight :: Num a => a
windowWidth = 640
windowHeight = 480

window :: Display
window = InWindow "Hello World" (windowWidth, windowHeight) (100, 100)

--------------------------
-- シミュレーションの実装
--------------------------

boxWidth, boxHeight :: Float
boxWidth  = 50
boxHeight = 50

data BoxState = BoxState
  { _x  :: Float -- x 座標の位置
  , _y  :: Float -- y 座標の位置
  , _vx :: Float -- x 方向の速度
  , _vy :: Float -- y 方向の速度
  }


initialBox :: BoxState
initialBox = BoxState 0 0 0 0

drawBox :: BoxState -> Picture
drawBox box = translate (_x box) (_y box) $ rectangleSolid boxWidth boxHeight

-- | イベントを処理する関数。EventKey以外のイベントは無視する
updateBox :: Event -> BoxState -> BoxState
updateBox (EventKey key ks _ _) box = updateBoxWithKey key ks box
updateBox (EventMotion _)       box = box
updateBox (EventResize _)       box = box

-- | 上下左右の速度を与える関数
up, down, right, left :: BoxState -> BoxState
up    box = box { _vy = _vy box + 100 }
down  box = box { _vy = _vy box - 100 }
right box = box { _vx = _vx box + 100 }
left  box = box { _vx = _vx box - 100 }

-- | 方向キーとWASDキーに対応して四角形を移動させる
updateBoxWithKey :: Key -> KeyState -> BoxState -> BoxState
updateBoxWithKey (SpecialKey KeyUp)    ks = if ks == Down then up    else down
updateBoxWithKey (SpecialKey KeyDown)  ks = if ks == Down then down  else up
updateBoxWithKey (SpecialKey KeyRight) ks = if ks == Down then right else left
updateBoxWithKey (SpecialKey KeyLeft)  ks = if ks == Down then left  else right
updateBoxWithKey (Char 'w')            ks = if ks == Down then up    else down
updateBoxWithKey (Char 's')            ks = if ks == Down then down  else up
updateBoxWithKey (Char 'd')            ks = if ks == Down then right else left
updateBoxWithKey (Char 'a')            ks = if ks == Down then left  else right
updateBoxWithKey _ _ = id

nextBox :: Float -> BoxState -> BoxState
nextBox dt box =
  let -- 速度を考慮した次のステップでの位置を計算
      x  = _x box + _vx box * dt
      y  = _y box + _vy box * dt

   in box { _x = x, _y = y }

-------------
-- main 関数
-------------

main :: IO ()
main = play window white 24 initialBox drawBox updateBox nextBox