FUNCTION zsd_int_ob_integra_contatos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_PARCEIRO) TYPE  KUNNR OPTIONAL
*"     VALUE(I_TP_CONTATO) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_NR_ROT) TYPE  Z_NR_ROT OPTIONAL
*"     VALUE(I_SOMENTE_CADASTRAR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_SOMENTE_ROTA) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_OUTROS_ENDERECOS) TYPE  CHAR01 OPTIONAL
*"  EXCEPTIONS
*"      ERRO_INTEGRACAO
*"----------------------------------------------------------------------

*-----------------------------------------------------------------------
* integrar contato  - SAFRA - valida se contato existe no SAFRA.
* Se nao existe, metodo = POST, senao metodo = PUT
*-----------------------------------------------------------------------
  IF i_parceiro IS NOT INITIAL.
*---cliente ---------------------------------------------------------
    FREE: t_zsdt0132, w_zsdt0132.

    IF i_tp_contato = 'C' OR i_tp_contato = abap_off.
      SELECT *
        FROM zsdt0132
        INTO TABLE t_zsdt0132
       WHERE kunnr = i_parceiro.

      w_zsdt0132-nr_rot  = 0.
      w_zsdt0132-kunnr   = i_parceiro.
      APPEND w_zsdt0132 TO t_zsdt0132.

      SORT t_zsdt0132 BY nr_rot.

      LOOP AT t_zsdt0132 INTO w_zsdt0132.
        PERFORM f_integrar_parceiros USING w_zsdt0132 i_somente_cadastrar.
      ENDLOOP.
    ENDIF.

*---fornecedor ------------------------------------------------------
    FREE: t_zsdt0132, w_zsdt0132.

    IF i_tp_contato = 'F' OR i_tp_contato = abap_off.
      SELECT *
        FROM zsdt0132
        INTO TABLE t_zsdt0132
       WHERE lifnr = i_parceiro.

      w_zsdt0132-nr_rot  = 0.
      w_zsdt0132-lifnr   = i_parceiro.
      APPEND w_zsdt0132 TO t_zsdt0132.

      SORT t_zsdt0132 BY nr_rot.

      LOOP AT t_zsdt0132 INTO w_zsdt0132.
        PERFORM f_integrar_parceiros USING w_zsdt0132 i_somente_cadastrar.
      ENDLOOP.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
* integrar Enderecos contato  - SAFRA - valida se contato existe no SAFRA.
* Se nao existe, metodo = POST, senao metodo = PUT
*-----------------------------------------------------------------------
  IF i_parceiro IS NOT INITIAL AND i_outros_enderecos = abap_true.
*---cliente ---------------------------------------------------------
    FREE: t_zsdt0132, w_zsdt0132.

    IF i_tp_contato = 'C' OR i_tp_contato = abap_off.
      SELECT *
        FROM zsdt0132
        INTO TABLE t_zsdt0132
       WHERE kunnr = i_parceiro.

      w_zsdt0132-nr_rot  = 0.
      w_zsdt0132-kunnr   = i_parceiro.
      APPEND w_zsdt0132 TO t_zsdt0132.

      SORT t_zsdt0132 BY nr_rot.

      LOOP AT t_zsdt0132 INTO w_zsdt0132.
        PERFORM f_integrar_enderecos USING w_zsdt0132 i_somente_cadastrar.
      ENDLOOP.
    ENDIF.

*---fornecedor ------------------------------------------------------
    FREE: t_zsdt0132, w_zsdt0132.

    IF i_tp_contato = 'F' OR i_tp_contato = abap_off.
      SELECT *
        FROM zsdt0132
        INTO TABLE t_zsdt0132
       WHERE lifnr = i_parceiro.

      w_zsdt0132-nr_rot  = 0.
      w_zsdt0132-lifnr   = i_parceiro.
      APPEND w_zsdt0132 TO t_zsdt0132.

      SORT t_zsdt0132 BY nr_rot.

      LOOP AT t_zsdt0132 INTO w_zsdt0132.
        PERFORM f_integrar_enderecos USING w_zsdt0132 i_somente_cadastrar.
      ENDLOOP.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
* integrar Rotas dos contatos - SAFRA - valida se contato existe no SAFRA.
* Se nao existe, metodo = POST, senao metodo = PUT
*-----------------------------------------------------------------------
  IF i_nr_rot IS NOT INITIAL.
    FREE: t_zsdt0132, w_zsdt0132.

    SELECT SINGLE *
      FROM zsdt0132
      INTO w_zsdt0132
     WHERE nr_rot = i_nr_rot.

    CHECK sy-subrc = 0.

    IF w_zsdt0132-kunnr IS NOT INITIAL.
      SELECT *
        FROM zsdt0132
        INTO TABLE t_zsdt0132
       WHERE kunnr = w_zsdt0132-kunnr.

      w_zsdt0132-nr_rot  = 0.
      w_zsdt0132-kunnr   = w_zsdt0132-kunnr.
      APPEND w_zsdt0132 TO t_zsdt0132.
    ELSE.
      SELECT *
        FROM zsdt0132
        INTO TABLE t_zsdt0132
       WHERE lifnr = w_zsdt0132-lifnr.

      w_zsdt0132-nr_rot  = 0.
      w_zsdt0132-lifnr   = w_zsdt0132-lifnr.
      APPEND w_zsdt0132 TO t_zsdt0132.
    ENDIF.

    IF i_somente_rota = abap_true.
      DELETE t_zsdt0132 WHERE nr_rot <> i_nr_rot.
    ENDIF.

    SORT t_zsdt0132 BY nr_rot.

    LOOP AT t_zsdt0132 INTO w_zsdt0132.
      PERFORM f_integrar_parceiros USING w_zsdt0132 i_somente_cadastrar.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
