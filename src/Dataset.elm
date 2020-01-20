module Dataset exposing (..)

import Set exposing (Set)

type alias Student =
    { name: String
    , visualizationSkill: Int
    , statisticsSkill: Int
    , mathematicsSkill: Int
    , artisticSkill: Int
    , computerSkill: Int
    , programmingSkill: Int
    , graphicsSkill: Int
    , hciSkill: Int
    , uxSkill: Int
    , communicationSkill: Int
    , collaborationSkill: Int
    , vcsSkill: Int
    , tags: Set String
    }