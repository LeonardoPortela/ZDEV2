FUNCTION z_les_itinerario_route.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_KUNNR) TYPE  KUNNR
*"     REFERENCE(P_LIFNR) TYPE  LIFNR
*"  EXPORTING
*"     VALUE(ROUTE_OUT) TYPE  ROUTE
*"  EXCEPTIONS
*"      NAO_CLI
*"      NAO_FOR
*"      NAO_CLI_ZONE
*"      NAO_FOR_ZONE
*"      NAO_ITINERA
*"      NAO_ZONAS
*"----------------------------------------------------------------------

  DATA: vl_adrnr_cl TYPE kna1-adrnr,
        vl_adrnr_fo TYPE kna1-adrnr,
        vl_trans_cl TYPE adrc-transpzone,
        vl_trans_fo TYPE adrc-transpzone,
        msg         TYPE string.

  CLEAR: msg, route_out.

  SELECT SINGLE adrnr
    FROM kna1
    INTO vl_adrnr_cl
  WHERE  kunnr EQ p_kunnr.

  IF sy-subrc EQ 0.
    SELECT SINGLE adrnr
      FROM lfa1
      INTO vl_adrnr_fo
    WHERE  lifnr EQ p_lifnr.
    IF sy-subrc NE 0.
      CONCATENATE 'Fornecedor' p_lifnr 'não localizado!' INTO msg SEPARATED BY space.
      MESSAGE i000 WITH msg RAISING nao_for.
    ENDIF.
  ELSE.
    CONCATENATE 'Cliente' p_kunnr 'não localizado!' INTO msg SEPARATED BY space.
    MESSAGE i000 WITH msg RAISING nao_cli.
  ENDIF.

  SELECT SINGLE transpzone
    FROM adrc
    INTO vl_trans_cl
   WHERE addrnumber EQ vl_adrnr_cl.
  IF ( sy-subrc EQ 0 ) AND ( NOT vl_trans_cl IS INITIAL ).
    SELECT SINGLE transpzone
      FROM adrc
      INTO vl_trans_fo
     WHERE addrnumber EQ vl_adrnr_fo.
    IF ( sy-subrc NE 0 ) OR ( vl_trans_fo IS INITIAL ).
      CONCATENATE 'Zona fornecedor' p_lifnr 'endereço' vl_adrnr_fo 'não localizado!' INTO msg SEPARATED BY space.
      MESSAGE i000 WITH msg RAISING nao_for_zona.
    ENDIF.
  ELSE.
    CONCATENATE 'Zona cliente' p_kunnr 'endereço' vl_adrnr_cl 'não localizado!' INTO msg SEPARATED BY space.
    MESSAGE i000 WITH msg RAISING nao_cli_zona.
  ENDIF.

  IF NOT vl_trans_cl IS INITIAL AND NOT vl_trans_fo IS INITIAL.
    SELECT SINGLE route
      FROM trolz
      INTO route_out
    WHERE  aland EQ 'BR'
      AND  azone EQ vl_trans_fo
      AND  lland EQ 'BR'
      AND  lzone EQ vl_trans_cl.

    IF route_out IS INITIAL.
      CONCATENATE 'O Itinerário não determinado' 'Z.Exp.' vl_adrnr_cl 'Z.Ent.' vl_adrnr_fo INTO msg SEPARATED BY space.
      MESSAGE i000 WITH msg RAISING nao_itinera.
    ELSE.
      CONCATENATE 'O Itinerário determinado' 'Z.Exp.' vl_adrnr_cl 'Z.Ent.' vl_adrnr_fo INTO msg SEPARATED BY space.
      MESSAGE s000 WITH msg.
    ENDIF.
  ELSE.
    MESSAGE i000 WITH 'O Itinerário não det. verif. zonas transporte' RAISING nao_zonas.
  ENDIF.

ENDFUNCTION.
