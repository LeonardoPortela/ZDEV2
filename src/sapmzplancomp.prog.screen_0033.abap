
PROCESS BEFORE OUTPUT.

  MODULE status_0033.

  "call subscreen: SUB0035 INCLUDING SY-CPROG VG_DYNNR_0035.


*
PROCESS AFTER INPUT.

  "call subscreen: sub0035.

  CHAIN.
    FIELD wa_filtro_remetente-centro.
    FIELD wa_filtro_remetente-ds_centro.
    FIELD wa_filtro_remetente-remetente.
    FIELD wa_filtro_remetente-ds_remetente.
    FIELD wa_filtro_remetente-data_ini.
    FIELD wa_filtro_remetente-data_fim.
    FIELD wa_filtro_remetente-grp_retorno.
    FIELD wa_filtro_remetente-numero_due.
    FIELD wa_filtro_remetente-regio.
    FIELD wa_filtro_remetente-regio_due.
    FIELD wa_filtro_remetente-codigo_ra_embarque.
*    FIELD wa_filtro_remetente-tipov.
*    FIELD wa_filtro_remetente-preco.
*    FIELD wa_filtro_remetente-depst.
*    FIELD wa_filtro_remetente-safra.
*    FIELD wa_filtro_remetente-cvirt.


  ENDCHAIN.

  FIELD: wa_filtro_remetente-cvirt MODULE zm_cvirt ON REQUEST.

  MODULE user_command_0033.

PROCESS ON VALUE-REQUEST.
  FIELD wa_filtro_remetente-numero_due MODULE busca_due_antecipada.
  FIELD wa_filtro_remetente-tipov MODULE busca_tipo.
  FIELD wa_filtro_remetente-depst MODULE busca_deposito.
