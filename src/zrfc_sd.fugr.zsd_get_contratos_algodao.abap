FUNCTION zsd_get_contratos_algodao .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_VENDA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_VENDA_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_CRIACAO_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_CRIACAO_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_SAFRA STRUCTURE  ZFIE_SAFRA
*"      T_CONTRATO STRUCTURE  ZFIE_CONTRATO
*"      T_SAIDA STRUCTURE  ZFIE_DADOS_CONTRATO_ALGO
*"      T_SAIDA_PRECOS STRUCTURE  ZSDT0227
*"----------------------------------------------------------------------

  DATA: lra_safra    TYPE RANGE OF zsdt0143-safra,
        lra_contrato TYPE RANGE OF zsdt0143-contrato,
        lra_dt_cria  TYPE RANGE OF zsdt0143-data_atual,
        lra_dt_venda TYPE RANGE OF zsdt0143-data_atual.

  IF i_data_venda_ini IS NOT INITIAL OR i_data_venda_fim IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'BT' low = i_data_venda_ini high = i_data_venda_fim ) TO lra_dt_venda.
  ENDIF.

  IF i_data_criacao_ini IS NOT INITIAL OR i_data_criacao_fim IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'BT' low = i_data_criacao_ini high = i_data_criacao_fim ) TO lra_dt_cria.
  ENDIF.

  LOOP AT t_safra ASSIGNING FIELD-SYMBOL(<fs_safra>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_safra>-safra ) TO lra_safra.
  ENDLOOP.

  LOOP AT t_contrato ASSIGNING FIELD-SYMBOL(<fs_contrato>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_contrato>-contrato ) TO lra_contrato.
  ENDLOOP.



  SELECT  c~id_contrato,
          c~contrato,
          c~contrato_cliente,
          c~safra,
          c~tp_venda,
          c~dt_venda,
          c~cliente,
          c~empresa,
          c~vendedor,
          c~corretor,
          c~comissao,
          c~tipo_padrao,
          r~maktx AS ds_produto,
          c~porto,
          c~quatidade,
          c~tolerancia,
          c~de_embarque,
          c~ate_embarque,
          c~preco,
          c~preco_tons,
          c~pctgem_ant,
          c~qdt_embarcada,
          c~sts_sol,
          c~sts_con,
          c~a_usnam,
          c~a_data_atual,
          c~a_hora_atual,
          c~cancelado,
          c~c_usnam,
          c~c_data_atual,
          c~c_hora_atual,
          c~t_cotton,
          c~t_usuario,
          c~t_data,
          c~t_hora,
          c~visao,
          c~intercompany,
          c~id_contrato_referencia,
          c~usnam,
          c~data_atual,
          c~hora_atual
        FROM zsdt0143 AS c LEFT OUTER JOIN makt AS r
        ON c~tipo_padrao = r~matnr
        INTO CORRESPONDING FIELDS OF TABLE @t_saida
        WHERE data_atual IN @lra_dt_cria
         AND dt_venda IN @lra_dt_venda
         AND safra IN @lra_safra
         AND contrato IN @lra_contrato.
  IF sy-subrc IS INITIAL.

    DATA(lt_saida) = t_saida[].
    SORT lt_saida BY id_contrato.
    DELETE ADJACENT DUPLICATES FROM lt_saida COMPARING id_contrato.

    SELECT *
      FROM zsdt0227
      INTO TABLE t_saida_precos
      FOR ALL ENTRIES IN lt_saida
     WHERE id_contrato = lt_saida-id_contrato.
  ENDIF.


ENDFUNCTION.
