module StatusCodes where

type Code = Int

data Status = Ok
            | Created
            | Accepted
            -- 4xx
            | BadRequest
            | Unauthorized
            | Forbidden
            | NotFound
            | MethodNotAllowed
            -- 5xx
            | InternalServerError
            | NotImplemented
            | BadGateway
            | ServiceUnavailable
            | GatewayTimeout
            deriving (Enum, Show, Ord, Eq)

data StatusCode = StatusCode Code Status deriving Show

-- 2xx
ok, created, accepted :: StatusCode
ok = StatusCode 200 Ok
created = StatusCode 201 Created
accepted = StatusCode 202 Accepted

-- 4xx
badRequest, unathorized, forbidden, notFound, methodNotAllowed :: StatusCode
badRequest = StatusCode 400 BadRequest
unathorized = StatusCode 401 Unauthorized
forbidden = StatusCode 403 Forbidden
notFound = StatusCode 404 NotFound
methodNotAllowed = StatusCode 405 MethodNotAllowed

-- 5xx
internalServerError, notImplemented, badGateway, serviceUnavailable, gatewayTimeout :: StatusCode
internalServerError = StatusCode 500 InternalServerError
notImplemented = StatusCode 501 NotImplemented
badGateway = StatusCode 502 BadGateway
serviceUnavailable = StatusCode 503 ServiceUnavailable
gatewayTimeout = StatusCode 504 GatewayTimeout

