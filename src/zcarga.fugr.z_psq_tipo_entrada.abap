FUNCTION z_psq_tipo_entrada.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ID_BUKRS) TYPE  BUKRS
*"     REFERENCE(ID_BRANCH) TYPE  J_1BBRANC_
*"     REFERENCE(NR_SAFRA) TYPE  ZDE_NR_SAFRA
*"  EXPORTING
*"     REFERENCE(TP_ENTRADA) TYPE  ZSDT0001TETX
*"  EXCEPTIONS
*"      ERRO
*"----------------------------------------------------------------------

  SELECT  DISTINCT
          tx~mandt,
          tx~id_entrada,
          tx~ds_entrada
     INTO TABLE @DATA(it_entrada)
     FROM zsdt0001te AS tx
    WHERE EXISTS ( SELECT *
                     FROM zsdt0001te AS cb
                    INNER JOIN zsdt0001teln AS ln
                            ON ln~id_entrada EQ cb~id_entrada
                           AND ln~id_bukrs   EQ cb~id_empresa
                    WHERE cb~id_entrada EQ tx~id_entrada
                      AND cb~id_empresa EQ @id_bukrs
                      AND ln~id_branch  EQ @id_branch
                      AND ln~nr_safra   EQ @nr_safra ).

  "146  Não Encontrado Tipo de Entrada p/ Empresa &1 Filial &2 Safra &3!
  IF sy-subrc IS NOT INITIAL.
    MESSAGE e146(zcarga) WITH id_bukrs id_branch nr_safra RAISING erro.
  ENDIF.

  DESCRIBE TABLE it_entrada LINES DATA(qtd_tipos).

  IF qtd_tipos EQ 1.
    READ TABLE it_entrada INTO DATA(wa_entrada) INDEX 1.
    tp_entrada-mandt = wa_entrada-mandt.
    tp_entrada-id_entrada = wa_entrada-id_entrada.
    tp_entrada-ds_entrada = wa_entrada-ds_entrada.
  ELSE.
    pid_bukrs  = id_bukrs.
    pid_branch = id_branch.
    pnr_safra  = nr_safra.
    it_tipo_entrada[] = it_entrada[].
    ck_selecionou     = abap_false.
    CALL SCREEN 9001 STARTING AT 10 05.
    IF ck_selecionou EQ abap_true.
      tp_entrada = wa_tipo_entrada.
    ENDIF.
  ENDIF.

  IF tp_entrada IS NOT INITIAL.

    SELECT SINGLE * FROM zsdt0001tetn INTO @DATA(tl_zsdt0001tetn)
      WHERE id_entrada EQ @wa_tipo_entrada-id_entrada.

    IF sy-subrc <> 0.
      MESSAGE e292(zcarga) WITH wa_tipo_entrada-id_entrada RAISING erro.
    ENDIF.

  ENDIF.



ENDFUNCTION.
