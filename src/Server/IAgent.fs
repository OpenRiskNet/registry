namespace Orn.Registry

type IAgent<'state, 'message> =
    abstract ReadonlyState: 'state
    abstract Post: 'message -> unit
