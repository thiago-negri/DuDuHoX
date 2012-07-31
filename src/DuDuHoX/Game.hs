module DuDuHoX.Game where

data MoveDirection = 
    MoveUp | 
    MoveDown | 
    MoveLeft | 
    MoveRight

data GameInput = 
    Movement MoveDirection | 
    Quit
