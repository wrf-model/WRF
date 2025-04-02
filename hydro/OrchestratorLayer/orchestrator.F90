module orchestrator_base
  use io_manager_base
  use config_base
  implicit none

  ! interface orchestrator_
  !    procedure orchestrator_init
  ! end interface orchestrator_

  type orchestrator_

     !class(FluxAggregator_) :: flux_aggregator
     !class(Groundwater_) :: groundwater
     !class(TimeManager_) :: time_manager
     type(IOManager_) :: IO_manager
     type(Configuration_) :: config
     !class(SpatialObject_) :: spatial_object

   contains

     procedure, public, pass(self) :: init => orchestrator_init

  end type orchestrator_

  type(orchestrator_), save :: orchestrator

contains

  !We may want routines to access the various components

  subroutine orchestrator_init(self)
    class (orchestrator_) :: self

    self%config = Configuration_()

    call self%config%init()
    ! Read configuration and decide how to assemble the various components
    ! Assuming IO_Manager_serial_ selected
    self%IO_manager = IOManager_()

  end subroutine orchestrator_init

end module orchestrator_base
