module DuDuHoX.Game where

data MoveDirection = 
    MoveUp | 
    MoveDown | 
    MoveLeft | 
    MoveRight |
    MoveUpLeft |
    MoveUpRight |
    MoveDownLeft |
    MoveDownRight

data GameInput = 
    Movement MoveDirection | 
    Quit
