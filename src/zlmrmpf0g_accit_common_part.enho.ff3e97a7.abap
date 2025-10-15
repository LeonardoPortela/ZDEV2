"Name: \PR:SAPLMRMP\FO:ACCIT_COMMON_PART_AUFBAUEN\SE:BEGIN\EI
ENHANCEMENT 0 ZLMRMPF0G_ACCIT_COMMON_PART.
* Atualização Documento FI - BBKO/Pathelle 08/06/2011
DATA vl_field TYPE char21 VALUE '(SAPLMR1M)RM08M-EBELN'.
FIELD-SYMBOLS: <ebeln> TYPE ekko-ebeln.
DATA:  OREF      TYPE REF TO ZCL_MEMORY_NFE_INBOUND_HANDLE.

if px_accit-ebeln is INITIAL.
  ASSIGN (vl_field) TO <ebeln>.
  IF <ebeln> IS ASSIGNED AND NOT
     <ebeln> IS INITIAL.
     IF pi_rbkpv-xrech NE space.
        px_accit-ebeln = <ebeln>.
     ENDIF.
  endif.
  UNASSIGN <ebeln>.
endif.

 if sy-tcode = 'MIRO'.
    px_accit-zuonr = px_accit-ebeln.
 elseif px_accit-LIFNR is not INITIAL.
      if px_accit-ebeln is INITIAL.
       IF WRXMOD-ebeln is not INITIAL.
          px_accit-ebeln = WRXMOD-ebeln.
       else.
          px_accit-ebeln = RBKPV-ZUONR.
       ENDIF.
    endif.
 endif.

** MIRO GERADA AUTOMAÇÃO
* TRY.
*    DATA(HANDLE) = ZCL_MEMORY_NFE_INBOUND=>ATTACH_FOR_READ( INST_NAME = CONV #( px_accit-LIFNR && px_accit-XBLNR ) ).
*    OREF ?= HANDLE->ROOT.
*    HANDLE->DETACH( ).
*    if px_accit-ebeln is INITIAL.
*       IF WRXMOD-ebeln is not INITIAL.
*          px_accit-ebeln = WRXMOD-ebeln.
*       else.
*          px_accit-ebeln = RBKPV-ZUONR.
*       ENDIF.
*    endif.
*  CATCH CX_SHM_ATTACH_ERROR.
* ENDTRY.

ENDENHANCEMENT.
