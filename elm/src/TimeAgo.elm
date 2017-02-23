module TimeAgo exposing (timeAgo)

import Date exposing (Date)
import Date.Extra.Duration as Duration
import Date.Extra.Period as Period
import String.Extra exposing (pluralize)


timeAgo : Date -> Date -> String
timeAgo date1 date2 =
    let
        periods =
            buildDatePeriods date1 date2

        hasPeriodElapsed ( number, _ ) =
            number > 0

        periodToUse =
            List.head (List.filter hasPeriodElapsed periods)
    in
        case periodToUse of
            Just ( number, period ) ->
                (pluralize period (period ++ "s") number) ++ " ago"

            Nothing ->
                "less than a minute ago"


buildDatePeriods : Date -> Date -> List ( Int, String )
buildDatePeriods date1 date2 =
    let
        durationDiff =
            Duration.diff date1 date2

        periodDiff =
            Period.diff date1 date2
    in
        [ ( durationDiff.year, "year" )
        , ( durationDiff.month, "month" )
        , ( periodDiff.week, "week" )
        , ( periodDiff.day, "day" )
        , ( periodDiff.hour, "hour" )
        , ( periodDiff.minute, "minute" )
        , ( periodDiff.second, "second" )
        ]
