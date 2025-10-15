*&---------------------------------------------------------------------*
*& Report ZTESTE_BIODIESEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zteste_biodiesel.

DATA: l_biodiesel TYPE char01.

PARAMETERS: p_chref  TYPE zch_ref.

TRY.
    l_biodiesel = zcl_faturamento=>zif_faturamento~get_instance(
                               )->set_transfere_biodiesel_frota( p_chref
                               ).

  CATCH zcx_error INTO DATA(lc_error).
    MESSAGE ID lc_error->msgid TYPE lc_error->msgty NUMBER lc_error->msgno WITH lc_error->msgv1 lc_error->msgv2 lc_error->msgv3 lc_error->msgv4
            DISPLAY LIKE 'E'.

ENDTRY.

*MESSAGE s024(sd) WITH 'TESTE'.
