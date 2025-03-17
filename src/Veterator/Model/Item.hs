module Veterator.Model.Item where

data Item = Item {itemName :: String, itemType :: ItemType, itemWeight :: Int}

data ItemType
  = Weapon {weaponDamageDice :: (Int, Int), weaponType :: WeaponType}
  | Armor {armorValue :: Int, equipSlot :: EquipSlot}
  | Jewelry {jewelryType :: JewelryType}
  | Food

data WeaponType = Spear | Sword | Axe | Bow

data JewelryType = Ring | Necklace

data EquipSlot = Head | Hands | Legs | Feet | Chest