method WDDOAFTERACTION .
endmethod.

method WDDOBEFOREACTION .
*  data lo_api_controller type ref to if_wd_view_controller.
*  data lo_action         type ref to if_wd_action.

*  lo_api_controller = wd_this->wd_get_api( ).
*  lo_action = lo_api_controller->get_current_action( ).

*  if lo_action is bound.
*    case lo_action->name.
*      when '...'.

*    endcase.
*  endif.
endmethod.

method WDDOEXIT .
endmethod.

method WDDOINIT .


*  DATA LO_CMP_USAGE TYPE REF TO IF_WD_COMPONENT_USAGE.
*
*  LO_CMP_USAGE =   WD_THIS->WD_CPUSE_ALV_SOLICITACAO( ).
*  IF LO_CMP_USAGE->HAS_ACTIVE_COMPONENT( ) IS INITIAL.
*    LO_CMP_USAGE->CREATE_COMPONENT( ).
*  ENDIF.



endmethod.

method WDDOMODIFYVIEW .
endmethod.

method WDDOONCONTEXTMENU .
endmethod.

