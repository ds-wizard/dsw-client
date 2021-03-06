module Public.ForgottenPassword.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form


type Msg
    = FormMsg Form.Msg
    | PostForgottenPasswordCompleted (Result ApiError ())
