class ZCL_IM_CL_FLUXO_EXPORTACAO definition
  public
  final
  create public .

public section.

  types:
    r_werks  TYPE RANGE OF werks_d .
  types:
    r_docnum TYPE RANGE OF j_1bdocnum .
  types:
    r_data   TYPE RANGE OF sydatum .

  class-data AT_CFOP_FIM_ESPECIFICOS type LXHME_RANGE_C10_T .
  class-data AT_CFOP_COMERCIALIZACAO type LXHME_RANGE_C10_T .
  class-data AT_GRUPO_MATERIAL type LXHME_RANGE_C10_T .
  class-data AT_CFOP_FORMACAO_LOTE type LXHME_RANGE_C10_T .
  class-data AT_CFOP_TRANSFERENCIA type LXHME_RANGE_C10_T .
  constants:
    BEGIN OF lc_contante,
        sign   TYPE c LENGTH 01 VALUE 'I',
        option TYPE c LENGTH 02 VALUE 'EQ',
        BEGIN OF acao,
          change    TYPE c LENGTH 06 VALUE 'CHANGE',
          insert    TYPE c LENGTH 06 VALUE 'INSERT',
          delete    TYPE c LENGTH 06 VALUE 'DELETE',
          upbalance TYPE c LENGTH 09 VALUE 'UPBALANCE',
          block     TYPE c LENGTH 05 VALUE 'BLOCK',
          back      TYPE c LENGTH 04 VALUE 'BACK',
          exit      TYPE c LENGTH 04 VALUE 'EXIT',
          cancel    TYPE c LENGTH 06 VALUE 'CANCEL',
        END OF acao,
        BEGIN OF operacao,
          incluir       TYPE c LENGTH 01 VALUE 'I',
          excluir       TYPE c LENGTH 01 VALUE 'E',
          alterar_saldo TYPE c LENGTH 01 VALUE 'S',
          bloqueiar     TYPE c LENGTH 01 VALUE 'B',
          desbloqueiar  TYPE c LENGTH 01 VALUE 'D',
          manutencao    TYPE c LENGTH 01 VALUE 'M',
        END OF operacao,
        BEGIN OF snum,
          seqflote TYPE c LENGTH 09 VALUE 'ZSEQFLOTE',
          seqprod  TYPE c LENGTH 08 VALUE 'ZSEQPROD',
        END OF snum,
      END OF lc_contante .
  class-data LS_PFLOTE type ZSDTPROD_FLOTE .
  class-data AT_CENTRO_REAL type ZSDT_DEPARA_CEN_T .
  class-data LS_FFLOTE type ZSDTFLOTE_FLOTE .

  class-methods BLOQUEIA_DESBLOQUEIA_NFE
    importing
      !I_NFE type ZNFE_XML_SEFAZ_AUTH
      !I_DOCNUM_EPROD type J_1BDOCNUM
      !I_BLOQ type ZDE_STATUS_BLOQ .
  class-methods PERMISSAO_ESTOQUE_NEGATIVO
    importing
      !MATNR type MATNR
      !BRANCH type J_1BBRANC_
    exporting
      !ESTOQUE_NEGATIVO type CHAR1 .
  class-methods CHECK_PROCESSO
    importing
      !I_CFOP type J_1BCFOP optional
      !I_DOCNUM type J_1BDOCNUM optional
      !I_WERKS type WERKS_D optional
    returning
      value(I_RESULT) type ZDE_NF_PROCESSO .
  class-methods GET_NFE_E_F
    importing
      !I_DOCNUM type J_1BDOCNUM optional
    changing
      !C_DOCNUM type R_DOCNUM optional
    returning
      value(R_NOTAS) type ZT_SD_NFE_E_F .
  class-methods GET_PROCESSO .
  class-methods CHECK_NFE_FILA_VINCULO
    importing
      !I_DOCNUM type R_DOCNUM optional
      !I_WERKS type R_WERKS optional
      !I_WERKS_REAL type R_WERKS optional
      !I_DATA_EMISSSAO type R_DATA optional
    returning
      value(R_FORMACAO_LOTE) type ZDESD_FORMACAO_LOTE .
  class-methods RETURN_NFE_VINC_F_LOTE
    importing
      !I_DOCNUM type R_DOCNUM optional
      !I_DATA type R_DATA optional
    returning
      value(R_VINCFLOTE) type ZSDTVINC_P_FLOTE_T .
  class-methods MANUT_FILA
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_OPERACAO type CHAR01
      !I_DOCNUM_FLOTE type J_1BDOCNUM optional
      !I_SALDO_VINC type ZSALDO_VINC optional
      !I_CANCEL type FLAG optional
      !I_FORMACAO_LOTE type ZSDTPROD_FLOTE optional
      !IS_FORMACAO_LOTE type ABAP_BOOL optional
      !I_MANUAL type CHAR1 default SPACE .
  class-methods CHECK_NEXT_SEQ
    importing
      !DOCNUM_VINC type J_1BDOCNUM optional
      !IS_FORMACAO_LOTE type ABAP_BOOL optional
    returning
      value(R_SEQUENCIA) type NRLEVEL .
  class-methods GET_CFOP
    importing
      !I_FORMACAO_LOTE type ABAP_BOOL optional
      !I_TRANSFERENCIA type ABAP_BOOL optional
      !I_COMERCIALIZACAO type ABAP_BOOL optional
      !I_FIM_ESPECIFICO type ABAP_BOOL optional
      !I_RETORNO_FORMACAO_LOTE type ABAP_BOOL optional
    returning
      value(R_CFOP) type LXHME_RANGE_C10_T .
  class-methods VINCULA_NFE_SAIDA_COM_ENTRADA
    importing
      !P_DOCNUM type J_1BDOCNUM .
  class-methods VALIDA_REGISTRO_FILA
    changing
      !C_ZSDTPROD_FLOTE_T type ZSDTPROD_FLOTE_T optional
      !C_ZSDTFLOTE_FLOTE_T type ZSDTFLOTE_FLOTE_T optional .
  class-methods SOL_APROVACAO_FLOTE
    importing
      !I_ATIVIDADE type CHAR1
      !I_DOCNUM_FLOTE type ZDOCNUM_FLOTE optional
      !I_BUKRS type BUKRS optional
      !IE_BNFLIN type J_1BNFLIN optional
      !I_MENGE_SALDO type MENGE_D optional
      !I_INTEGRA type FC_SBINT optional
      !I_ROMANEIO_COMPLETO type ZROMANEIO_COMPLETO optional
      !I_EUDR type ZEUDR optional
    exporting
      !ET_VINC_F_APROV type ZSDCTVINC_F_APROV
      !ET_ZSDT0392 type ZSDCT0392 .
  class-methods EMAIL_APROVACAO
    importing
      !IT_VINC_F_APROV type ZSDCTVINC_F_APROV
      !I_BUKRS type BUKRS
      !I_STATUS type ZSDESTATUS default SPACE .
  class-methods GET_TEXTOS_FLXEXP_NF_SAIDA
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_TEXTOS) type STRING .
*  class-methods MANUT_FILA .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_IM_CL_FLUXO_EXPORTACAO IMPLEMENTATION.


  METHOD bloqueia_desbloqueia_nfe.

    DATA: lo_util       TYPE REF TO zcl_util,
          lt_documentos TYPE j_1b_tt_nfe_active,
          lv_chave      TYPE zde_chave_nfe,
          lt_bloq       TYPE TABLE OF zsdtprod_bloq,
          lv_iedest     TYPE c,
          lv_bukrs      TYPE bukrs,
          lv_branch     TYPE j_1bbranc_.

    CASE i_bloq.
      WHEN 'A'.

        CREATE OBJECT lo_util.

        lv_chave = i_nfe-nfeproc-protnfe-infprot-chnfe.

        CALL METHOD lo_util->get_docnum
          EXPORTING
            i_chave_nfe  = lv_chave
          RECEIVING
            t_documentos = lt_documentos.

        IF lt_documentos IS NOT INITIAL.
          SELECT *
            FROM zsdtvinc_p_flote
            INTO TABLE @DATA(lt_vinc)
            FOR ALL ENTRIES IN @lt_documentos
            WHERE docnum_eprod EQ @lt_documentos-docnum
              AND cancel       EQ @abap_false.
          IF sy-subrc IS INITIAL.

            LOOP AT lt_vinc ASSIGNING FIELD-SYMBOL(<fs_vinc>).
              APPEND INITIAL LINE TO lt_bloq ASSIGNING FIELD-SYMBOL(<fs_bloq>).

              <fs_bloq>-chave_nfe      = lv_chave.
              <fs_bloq>-docnum_flote   = <fs_vinc>-docnum_flote.
              <fs_bloq>-qtd_vinc_flote = <fs_vinc>-qtd_vinc.
              <fs_bloq>-status         = 'A'.
              <fs_bloq>-forne_cnpj     = i_nfe-nfeproc-nfe-infnfe-emit-cnpj.

              READ TABLE lt_documentos ASSIGNING FIELD-SYMBOL(<fs_documentos>) INDEX 1.
              IF sy-subrc IS INITIAL.
                <fs_bloq>-bukrs          = <fs_documentos>-bukrs.
                <fs_bloq>-branch         = <fs_documentos>-branch.
              ENDIF.

              <fs_bloq>-usuario        = sy-uname.
              <fs_bloq>-data           = sy-datum.
              <fs_bloq>-hora           = sy-uzeit.

            ENDLOOP.

            IF lt_bloq IS NOT INITIAL.
              MODIFY zsdtprod_bloq FROM TABLE lt_bloq.
            ENDIF.

          ENDIF.

        ENDIF.

      WHEN 'I'.

        IF i_docnum_eprod IS NOT INITIAL.

          UPDATE zsdtprod_bloq SET status = 'I'
                                   usuario_inativo = sy-uname
                                   data_inativo    = sy-datum
                                   hora_inativo    = sy-uzeit
                             WHERE docnum_flote = i_docnum_eprod.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.


  METHOD check_next_seq.

    DATA: lv_returncode TYPE nrreturn.
    DATA: lv_obj TYPE nrobj.

    lv_obj = COND #( WHEN is_formacao_lote IS INITIAL
                              THEN lc_contante-snum-seqprod
                              ELSE lc_contante-snum-seqflote ).

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = CONV nrnr( '01' )
        object                  = lv_obj
      IMPORTING
        number                  = r_sequencia
        returncode              = lv_returncode
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

  ENDMETHOD.


  METHOD check_nfe_fila_vinculo.
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 04/03/2024|DEVK9A1XAW |NSEGATIN       | Atualiza campo Manual da tabela ZSDTPROC_FLOTE de Criação de  *
*                                        |Vinculo manuala da ZSDT0219 (Manutenção - Fluxo de Exportação).*
*                                        |BUG Solto 167326.                                              *
*--------------------------------------------------------------------------------------------------------*

    DATA: r_docnum         TYPE RANGE OF j_1bdocnum,
          it_formacao_lote TYPE zsdtprod_flote_t.

    FREE r_formacao_lote.

    SELECT  docnum,
            itmnum,
            werks,
            sequencial AS seq,
            grupomercadoria AS matkl,
            werksreal AS werks_real,
            dataemissao AS data_emissao,
            material,
            qtdnotafiscal AS qtd_nf,
            saldovinculado AS saldo_vinc,
            saldodisponivel AS saldo_disponivel,
            saldonaodisponivel AS saldo_nao_disponivel,
            comprafimespecifico AS compra_fim_es,
            romaneiocompleto AS romaneio_completo,
            eudr,
            entradatransferencia AS entrada_transf,
            usuariocriacao AS us_criacao,
            datacriacao AS dt_criacao,
            horacriacao AS hr_criacao,
            manual,                                      "<<<------"167326 - NMS------>>>
            cancel,
            usuariocancel AS us_cancel,
            datacancel AS dt_cancel,
            horacancel AS hr_cancel
      FROM zisd_formacao_lote
      INTO CORRESPONDING FIELDS OF TABLE @it_formacao_lote
      WHERE docnum IN @i_docnum
      AND werks IN @i_werks
      AND werksreal IN @i_werks_real
      AND dataemissao IN @i_data_emisssao.

*    DELETE it_formacao_lote WHERE cancel EQ abap_true AND us_cancel NE 'BLOQUEADO'.

    r_formacao_lote-zsdtprod_flote = CONV #( it_formacao_lote ).

    return_nfe_vinc_f_lote(
      EXPORTING
        i_docnum    = i_docnum
        i_data      = i_data_emisssao
      RECEIVING
        r_vincflote = DATA(lt_vincflote)
    ).

    r_formacao_lote-zsdtvinc_p_flote = lt_vincflote.

  ENDMETHOD.


  METHOD check_processo.

    IF i_cfop IS NOT INITIAL.
*"// CFOP Entrada Fins Exportação
      IF at_cfop_fim_especificos IS NOT INITIAL AND
        i_cfop IN at_cfop_fim_especificos.
        i_result-fins_especifico = abap_true.
      ENDIF.

*"// CFOP Comercialização
      IF at_cfop_comercializacao IS NOT INITIAL AND
        i_cfop IN at_cfop_comercializacao.
        i_result-comercializacao = abap_true.
      ENDIF.

*"// CFOP Formação de lote
      APPEND LINES OF at_cfop_transferencia TO at_cfop_formacao_lote.
      IF at_cfop_formacao_lote IS NOT INITIAL AND
        i_cfop IN at_cfop_formacao_lote.
        i_result-formacao_lote = abap_true.
      ENDIF.

*"// CFOP Entrada por Transferência
      IF at_cfop_transferencia IS NOT INITIAL AND
        i_cfop IN at_cfop_transferencia.
        i_result-transferencia = abap_true.
        i_result-formacao_lote = abap_false.
      ENDIF.
    ENDIF.


    IF i_docnum IS NOT INITIAL.

*"// EUDR
      CALL METHOD zcl_eudr_utils=>check_doc_fiscal_eudr
        EXPORTING
          i_docnum = i_docnum
        RECEIVING
          r_eudr   = DATA(is_eudr).

      IF is_eudr EQ 'S'.
        i_result-eudr = abap_true.
      ENDIF.

*"// Romaneio Completo
      zcl_les_utils=>check_doc_fiscal_rom_completo(
        EXPORTING
          i_docnum              = i_docnum
        IMPORTING
          e_docnum_rom_completo = DATA(lit_docnum_rom_completo) ).

      IF line_exists( lit_docnum_rom_completo[ docnum = i_docnum ] ).
        i_result-romaneio_completo = abap_true.
      ENDIF.

    ENDIF.

*"// Centro Real
    IF i_werks IS NOT INITIAL.
      READ TABLE at_centro_real INTO DATA(ls_centro_real) WITH KEY centrov_1 = i_werks.
      IF sy-subrc IS INITIAL.
        i_result-centro_real = ls_centro_real-centro_real.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD get_nfe_e_f.

    CHECK i_docnum IS NOT INITIAL OR c_docnum IS NOT INITIAL.

    IF i_docnum IS NOT INITIAL.
      APPEND
      VALUE #(
               sign    = zcl_les_utils=>if_stab_constants~mc_sign_include
               option  = zcl_les_utils=>if_stab_constants~mc_option_equal
               low     = i_docnum
              ) TO c_docnum.
    ENDIF.

    CHECK c_docnum IS NOT INITIAL.

    get_processo( ).

    SELECT *
      FROM zi_sd_nfe_e_f
      INTO CORRESPONDING FIELDS OF TABLE @r_notas
      FOR ALL ENTRIES IN @c_docnum
      WHERE docnum EQ @c_docnum-low.

    LOOP AT r_notas ASSIGNING FIELD-SYMBOL(<fs_notas>).

      check_processo(
        EXPORTING
          i_cfop   = <fs_notas>-cfop
          i_docnum = CONV #( <fs_notas>-docnum )
          i_werks  = <fs_notas>-werks
        RECEIVING
          i_result = DATA(ls_result)
      ).

      <fs_notas>-comprafimespecifico  = ls_result-fins_especifico.
      <fs_notas>-eudr                 = ls_result-eudr.
      <fs_notas>-entradatransferencia = ls_result-transferencia.
      <fs_notas>-formacaolote         = ls_result-formacao_lote.
      <fs_notas>-romaneiocompleto     = ls_result-romaneio_completo.
      <fs_notas>-centro_real          = ls_result-centro_real.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_processo.

*"// CFOP Fins Especifico
    get_cfop(
      EXPORTING
        i_fim_especifico = abap_true
      RECEIVING
        r_cfop           = at_cfop_fim_especificos
    ).

*"// CFOP Comercialização
    get_cfop(
      EXPORTING
        i_comercializacao = abap_true
      RECEIVING
        r_cfop            = at_cfop_comercializacao
    ).

*"// Grupo de materiais
    SELECT 'I'   AS sign,
           'EQ'  AS option,
           matkl AS low
      FROM zsdt0356
      INTO TABLE @at_grupo_material
    WHERE excluido EQ @abap_false.

*"// CFOP Formação de lote
    get_cfop(
      EXPORTING
        i_formacao_lote = abap_true
      RECEIVING
        r_cfop          = at_cfop_formacao_lote
    ).

*"// CFOP Entrada por Transferência
    get_cfop(
      EXPORTING
        i_transferencia = abap_true
      RECEIVING
        r_cfop          = at_cfop_transferencia
    ).

*"// Centro Real
    SELECT vkorg, centro_real, centrov_1, tp_centro_virtual
      FROM zsdt_depara_cen
      INTO CORRESPONDING FIELDS OF TABLE @at_centro_real.


  ENDMETHOD.


  METHOD manut_fila.
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 04/03/2024|DEVK9A1XAW |NSEGATIN       | Atualiza campo Manual da tabela ZSDTPROC_FLOTE de Criação de  *
*                                        |Vinculo manuala da ZSDT0219 (Manutenção - Fluxo de Exportação).*
*                                        |BUG Solto 167326.                                              *
*--------------------------------------------------------------------------------------------------------*
*& 27/03/2025|DEVK9A1XAW |NSEGATIN       | Ajsute na marcação do campo Saldo não Disponível quando Saldo *
*                                        |Disponível for igual a Zero. BUG 172407.                       *
*--------------------------------------------------------------------------------------------------------*

    DATA: ls_flote TYPE zsdtprod_flote,
          lo_util  TYPE REF TO zcl_util.

    get_nfe_e_f(
      EXPORTING
        i_docnum = i_docnum
      RECEIVING
        r_notas  = DATA(r_notas)
    ).

    READ TABLE r_notas INTO DATA(ls_notas) INDEX 1.

    CASE i_operacao.
      WHEN lc_contante-operacao-incluir.

        LOOP AT r_notas INTO ls_notas.

          IF ls_notas-formacaolote IS NOT INITIAL.
            ls_fflote =
            VALUE #(
                     docnum     = i_formacao_lote-docnum
                     itmnum     = i_formacao_lote-itmnum
                     werks      = i_formacao_lote-werks
                     seq        = check_next_seq( is_formacao_lote = abap_true )
                     werks_real = i_formacao_lote-werks_real
                     data_emissao = i_formacao_lote-data_emissao
                     material   = i_formacao_lote-material
*                     matkl = i_formacao_lote-matkl
                     qtd_nf = i_formacao_lote-qtd_nf
                     saldo_vinc  = i_formacao_lote-saldo_vinc
                     saldo_disponivel  = i_formacao_lote-saldo_disponivel
                     saldo_nao_disponivel  = i_formacao_lote-saldo_nao_disponivel
                     compra_fim_es = ls_notas-comprafimespecifico
                     eudr       = ls_notas-eudr
                     us_criacao = sy-uname
                     dt_criacao = sy-datum
                     hr_criacao = sy-uzeit
                     cancel     = abap_true
                     us_cancel  = 'BLOQUEADO'
                     dt_cancel  = sy-datum
                     hr_cancel  = sy-uzeit
                  ).

            INSERT zsdtflote_flote FROM ls_fflote.

          ELSE.

            ls_pflote =
            VALUE #(
                     docnum     = i_formacao_lote-docnum
                     itmnum     = i_formacao_lote-itmnum
                     werks      = i_formacao_lote-werks
                     seq        = check_next_seq( is_formacao_lote = abap_false )
                     werks_real = i_formacao_lote-werks_real
                     data_emissao = i_formacao_lote-data_emissao
                     material   = i_formacao_lote-material
                     matkl = ls_notas-matkl
                     qtd_nf = i_formacao_lote-qtd_nf
                     saldo_vinc  = i_formacao_lote-saldo_vinc
                     saldo_disponivel  = i_formacao_lote-saldo_disponivel
                     saldo_nao_disponivel  = i_formacao_lote-saldo_nao_disponivel
                     compra_fim_es = ls_notas-comprafimespecifico
                     romaneio_completo = ls_notas-romaneiocompleto
                     entrada_transf = ls_notas-entradatransferencia
                     eudr       = ls_notas-eudr
                     us_criacao = sy-uname
                     dt_criacao = sy-datum
                     hr_criacao = sy-uzeit
                     cancel     = abap_true
                     us_cancel  = 'BLOQUEADO'
                     dt_cancel  = sy-datum
                     hr_cancel  = sy-uzeit
                  ).

            INSERT zsdtprod_flote FROM ls_pflote.

          ENDIF.
        ENDLOOP.

        MESSAGE 'Linha incluida com Sucesso!' TYPE 'S'.

      WHEN lc_contante-operacao-excluir.

        IF ls_notas-formacaolote IS NOT INITIAL.

          UPDATE zsdtflote_flote SET cancel    = abap_true
                                     us_cancel = sy-uname
                                     dt_cancel = sy-datum
                                     hr_cancel = sy-uzeit
             WHERE docnum EQ i_formacao_lote-docnum
               AND itmnum EQ i_formacao_lote-itmnum
               AND seq    EQ i_formacao_lote-seq.

        ELSE.

          UPDATE zsdtprod_flote SET cancel    = abap_true
                                    us_cancel = sy-uname
                                    dt_cancel = sy-datum
                                    hr_cancel = sy-uzeit
                      WHERE docnum EQ i_formacao_lote-docnum
                        AND itmnum EQ i_formacao_lote-itmnum
                        AND werks  EQ i_formacao_lote-werks
                        AND seq    EQ i_formacao_lote-seq.

        ENDIF.

      WHEN lc_contante-operacao-alterar_saldo.

        manut_fila(
          i_docnum        = i_docnum
          i_operacao      = lc_contante-operacao-excluir
          i_formacao_lote = i_formacao_lote
        ).

        manut_fila(
          i_docnum        = i_docnum
          i_operacao      = lc_contante-operacao-incluir
          i_formacao_lote = i_formacao_lote
        ).

      WHEN lc_contante-operacao-bloqueiar.

        IF i_formacao_lote-us_cancel EQ 'BLOQUEADO'.
          CLEAR ls_flote.
        ELSE.
          ls_flote-cancel = abap_true.
          ls_flote-us_cancel = 'BLOQUEADO'.
          ls_flote-dt_cancel = sy-datum.
          ls_flote-hr_cancel = sy-uzeit.
        ENDIF.

        IF ls_notas-formacaolote IS NOT INITIAL.

          UPDATE zsdtflote_flote SET cancel    = ls_flote-cancel
                                     us_cancel = ls_flote-us_cancel
                                     dt_cancel = ls_flote-dt_cancel
                                     hr_cancel = ls_flote-hr_cancel
             WHERE docnum EQ i_formacao_lote-docnum
               AND itmnum EQ i_formacao_lote-itmnum
               AND seq    EQ i_formacao_lote-seq.

        ELSE.

          UPDATE zsdtprod_flote SET cancel    = ls_flote-cancel
                                    us_cancel = ls_flote-us_cancel
                                    dt_cancel = ls_flote-dt_cancel
                                    hr_cancel = ls_flote-hr_cancel
             WHERE docnum EQ i_formacao_lote-docnum
               AND itmnum EQ i_formacao_lote-itmnum
               AND werks  EQ i_formacao_lote-werks
               AND seq    EQ i_formacao_lote-seq.

        ENDIF.

      WHEN lc_contante-operacao-manutencao.

        IF i_cancel IS NOT INITIAL.

          IF ls_notas-formacaolote IS NOT INITIAL.

            SELECT *
              FROM zsdtflote_flote
              INTO TABLE @DATA(lt_flote)
              WHERE docnum = @i_docnum
                AND ( cancel = @abap_false
                 OR   us_cancel = 'BLOQUEADO' ).
            IF sy-subrc IS INITIAL.
              READ TABLE lt_flote ASSIGNING FIELD-SYMBOL(<fs_flote>) INDEX 1.
              IF sy-subrc IS INITIAL.
                UPDATE zsdtflote_flote SET cancel    = abap_true
                                           us_cancel = sy-uname
                                           dt_cancel = sy-datum
                                           hr_cancel = sy-uzeit
                  WHERE docnum EQ <fs_flote>-docnum
                    AND itmnum EQ <fs_flote>-itmnum
                    AND seq    EQ <fs_flote>-seq.

                DATA(lv_seq) = zcl_im_cl_fluxo_exportacao=>check_next_seq( EXPORTING is_formacao_lote = abap_true ).

                <fs_flote>-seq = lv_seq.
                <fs_flote>-saldo_disponivel = <fs_flote>-saldo_disponivel + i_saldo_vinc.
                <fs_flote>-saldo_vinc = <fs_flote>-saldo_vinc - i_saldo_vinc.
                <fs_flote>-us_criacao = sy-uname.
                <fs_flote>-dt_criacao = sy-datum.
                <fs_flote>-hr_criacao = sy-uzeit.
                CLEAR: <fs_flote>-saldo_nao_disponivel,
                        <fs_flote>-us_cancel,
                        <fs_flote>-dt_cancel,
                        <fs_flote>-cancel,
                        <fs_flote>-hr_cancel.

                MODIFY zsdtflote_flote FROM <fs_flote>.

              ENDIF.

            ENDIF.

          ELSE.

            SELECT *
              FROM zsdtprod_flote
              INTO TABLE @DATA(lt_prod)
              WHERE docnum = @i_docnum
                AND ( cancel = @abap_false
                 OR   us_cancel = 'BLOQUEADO' ).
            IF sy-subrc IS INITIAL.
              READ TABLE lt_prod ASSIGNING FIELD-SYMBOL(<fs_prod>) INDEX 1.
              IF sy-subrc IS INITIAL.
                UPDATE zsdtprod_flote SET cancel    = abap_true
                                          us_cancel = sy-uname
                                          dt_cancel = sy-datum
                                          hr_cancel = sy-uzeit
                  WHERE docnum EQ <fs_prod>-docnum
                    AND itmnum EQ <fs_prod>-itmnum
                    AND seq    EQ <fs_prod>-seq.

                lv_seq = zcl_im_cl_fluxo_exportacao=>check_next_seq( EXPORTING is_formacao_lote = abap_false ).

                <fs_prod>-seq              = lv_seq.
                <fs_prod>-saldo_disponivel = <fs_prod>-saldo_disponivel + i_saldo_vinc.
                <fs_prod>-saldo_vinc = <fs_prod>-saldo_vinc - i_saldo_vinc.
                <fs_prod>-us_criacao = sy-uname.
                <fs_prod>-dt_criacao = sy-datum.
                <fs_prod>-hr_criacao = sy-uzeit.
**<<<------"167326 - NMS - INI------>>>
                <fs_prod>-manual     = i_manual.
**<<<------"172407 - NMS - INI------>>>
                IF <fs_prod>-saldo_disponivel IS INITIAL.
                  <fs_prod>-saldo_nao_disponivel = abap_on.

                ELSE.
                  CLEAR <fs_prod>-saldo_nao_disponivel.

                ENDIF.
**<<<------"172407 - NMS - FIM------>>>
                IF <fs_prod>-us_cancel NE 'BLOQUEADO'.
**<<<------"167326 - NMS - FIM------>>>
                  CLEAR: <fs_prod>-saldo_nao_disponivel,
                         <fs_prod>-us_cancel,
                         <fs_prod>-dt_cancel,
                         <fs_prod>-cancel,
                         <fs_prod>-hr_cancel.
*******<<<------"167326 - NMS - INI------>>>
                ENDIF.
*******<<<------"167326 - NMS - FIM------>>>
                MODIFY zsdtprod_flote FROM <fs_prod>.
              ENDIF.

            ENDIF.

          ENDIF.

          SELECT *
            FROM zsdtvinc_p_flote
            INTO @DATA(ls_vinc)
            UP TO 1 ROWS
            WHERE docnum_eprod = @i_docnum
              AND docnum_flote = @i_docnum_flote
              AND cancel       = @abap_false.
          ENDSELECT.
          IF sy-subrc IS INITIAL.

            ls_vinc-us_cancel = sy-uname.
            ls_vinc-dt_cancel = sy-datum.
            ls_vinc-hr_cancel = sy-uzeit.
            ls_vinc-cancel    = abap_true.

            MODIFY zsdtvinc_p_flote FROM ls_vinc.

          ENDIF.

        ELSE.

          IF ls_notas-formacaolote IS NOT INITIAL.

            SELECT *
              FROM zsdtflote_flote
              INTO TABLE lt_flote
             WHERE docnum = i_docnum
               AND ( us_cancel = 'BLOQUEADO'
                OR   cancel = abap_false ).
            IF sy-subrc IS INITIAL.
              READ TABLE lt_flote ASSIGNING <fs_flote> INDEX 1.
              IF sy-subrc IS INITIAL.

                UPDATE zsdtflote_flote SET cancel    = abap_true
                                           us_cancel = sy-uname
                                           dt_cancel = sy-datum
                                           hr_cancel = sy-uzeit
                                  WHERE docnum EQ <fs_flote>-docnum
                                    AND itmnum EQ <fs_flote>-itmnum
                                    AND seq    EQ <fs_flote>-seq.

                lv_seq = zcl_im_cl_fluxo_exportacao=>check_next_seq( EXPORTING is_formacao_lote = abap_true ).

                <fs_flote>-seq = lv_seq.
                <fs_flote>-saldo_disponivel = <fs_flote>-saldo_disponivel - i_saldo_vinc.
                <fs_flote>-saldo_vinc       = <fs_flote>-saldo_vinc + i_saldo_vinc.
                <fs_flote>-us_criacao = sy-uname.
                <fs_flote>-dt_criacao = sy-datum.
                <fs_flote>-hr_criacao = sy-uzeit.

                MODIFY zsdtflote_flote FROM <fs_flote>.
              ENDIF.

            ENDIF.

          ELSE.

            SELECT *
              FROM zsdtprod_flote
              INTO TABLE lt_prod
             WHERE docnum = i_docnum
               AND ( us_cancel = 'BLOQUEADO'
                OR   cancel = abap_false ).
            IF sy-subrc IS INITIAL.
              READ TABLE lt_prod ASSIGNING <fs_prod> INDEX 1.
              IF sy-subrc IS INITIAL.
                UPDATE zsdtprod_flote SET cancel    = abap_true
                                           us_cancel = sy-uname
                                           dt_cancel = sy-datum
                                           hr_cancel = sy-uzeit
                  WHERE docnum EQ <fs_prod>-docnum
                    AND itmnum EQ <fs_prod>-itmnum
                    AND seq    EQ <fs_prod>-seq.

                lv_seq = zcl_im_cl_fluxo_exportacao=>check_next_seq( EXPORTING is_formacao_lote = abap_false ).

                <fs_prod>-seq              = lv_seq.
**<<<------"167326 - NMS - INI------>>>
                IF i_manual IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
                  <fs_prod>-saldo_disponivel = <fs_prod>-saldo_disponivel - i_saldo_vinc.
                  <fs_prod>-saldo_vinc       = <fs_prod>-saldo_vinc + i_saldo_vinc.
**<<<------"167326 - NMS - INI------>>>
                ELSE.
                  <fs_prod>-saldo_disponivel = <fs_prod>-saldo_disponivel + ( <fs_prod>-saldo_vinc - i_saldo_vinc ).
                  <fs_prod>-saldo_vinc       = i_saldo_vinc.

                ENDIF.
**<<<------"172407 - NMS - INI------>>>
                IF <fs_prod>-saldo_disponivel IS INITIAL.
                  <fs_prod>-saldo_nao_disponivel = abap_on.

                ELSE.
                  CLEAR <fs_prod>-saldo_nao_disponivel.

                ENDIF.
**<<<------"172407 - NMS - FIM------>>>

**<<<------"167326 - NMS - FIM------>>>
                <fs_prod>-us_criacao = sy-uname.
                <fs_prod>-dt_criacao = sy-datum.
                <fs_prod>-hr_criacao = sy-uzeit.
                <fs_prod>-manual     = i_manual. "<<<------"167326 - NMS------>>>

                MODIFY zsdtprod_flote FROM <fs_prod>.
              ENDIF.

            ENDIF.

          ENDIF.

          CREATE OBJECT lo_util.

          SELECT regio
            FROM zsdt0363
            INTO TABLE @DATA(lt_uf)
            WHERE excluido EQ @space.
          IF sy-subrc IS INITIAL.
            SORT lt_uf BY regio.
          ENDIF.

          SELECT SINGLE regio
            FROM j_1bnfdoc
            INTO @DATA(lv_regio)
            WHERE docnum = @i_docnum_flote.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_uf TRANSPORTING NO FIELDS
            WITH KEY regio = lv_regio
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              DATA(lv_regio_ok) = abap_true.
            ENDIF.

          ENDIF.

          SELECT MAX( id_vinc )
            FROM zsdtvinc_p_flote
            INTO @DATA(lv_id_vinc)
            WHERE docnum_flote = @i_docnum_flote
              AND docnum_eprod = @i_docnum.
          IF lv_seq IS NOT INITIAL AND lv_seq > 0.
**<<<------"167326 - NMS - INI------>>>
            IF NOT i_manual   IS INITIAL AND
               NOT lv_id_vinc IS INITIAL.
              UPDATE zsdtvinc_p_flote
                 SET cancel    = abap_on
                     us_cancel = sy-uname
                     dt_cancel = sy-datlo
                     hr_cancel = sy-timlo
              WHERE docnum_flote EQ i_docnum_flote
                AND docnum_eprod EQ i_docnum
                AND id_vinc      EQ lv_id_vinc.

            ENDIF.
**<<<------"167326 - NMS - FIM------>>>
            ls_vinc-id_vinc = lv_id_vinc + 1.
          ELSE.
            ls_vinc-id_vinc = 1.
          ENDIF.

          ls_vinc-mandt        = sy-mandt.
          ls_vinc-docnum_flote = i_docnum_flote.
          ls_vinc-docnum_eprod = i_docnum.
          ls_vinc-qtd_vinc     = i_saldo_vinc.
          ls_vinc-us_criacao   = sy-uname.
          ls_vinc-dt_criacao   = sy-datum.
          ls_vinc-hr_criacao   = sy-uzeit.
          ls_vinc-chave_nfe    = lo_util->get_chave_nfe( i_docnum ).
          ls_vinc-manual       = i_manual. "<<<------"167326 - NMS------>>>

          MODIFY zsdtvinc_p_flote FROM ls_vinc.

        ENDIF.

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD permissao_estoque_negativo.

    CLEAR: estoque_negativo.

    zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
      EXPORTING
        i_material      = matnr
        i_centro_fixo   = branch
      IMPORTING
        e_single_depara = DATA(wa_zmmt0017)
    ).

    IF wa_zmmt0017 IS INITIAL.
      SELECT SINGLE *
        FROM t001k
        INTO @DATA(ls_t001k)
        WHERE bwkey = @branch.
      IF sy-subrc IS INITIAL.
        IF ls_t001k-xbkng IS not INITIAL.
          estoque_negativo = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT *
      FROM zmm0023 INTO TABLE @DATA(lt_0023)
     WHERE werks  = @branch
       AND matnr  = @matnr.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE matkl
        FROM mara INTO @DATA(lv_matkl)
       WHERE matnr = @matnr.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM zmm0023 INTO TABLE lt_0023
         WHERE werks  = branch
           AND matkl  = lv_matkl.
      ENDIF.
    ENDIF.

    IF lt_0023 IS NOT INITIAL.
      SORT lt_0023 BY werks ASCENDING matnr ASCENDING matkl ASCENDING cwerks DESCENDING.

      READ TABLE lt_0023 ASSIGNING FIELD-SYMBOL(<fs_0023>) INDEX 1.
      IF sy-subrc IS INITIAL.

        IF <fs_0023>-status EQ 'A'.
          estoque_negativo = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD return_nfe_vinc_f_lote.
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 05/02/2025|DEVK9A1XAW |NSEGATIN       | Ajuste na consulta da Tabela de vinculo entre a formação de   *
*&                                       | lote (ZSDTVINC_P_FLOTE) para quando o parâmetro I_DOCNUM for  *
*&                                       | vazio, não realizar a segunda consulta evitando duplicidade de*
*&                                       | registros. Chamado 165575.                                    *
*--------------------------------------------------------------------------------------------------------*
*& 06/03/2024|DEVK9A1XAW |NSEGATIN       | Atualiza campo Manual da tabela ZSDTPROC_FLOTE de Criação de  *
*&                                       |Vinculo manuala da ZSDT0219 (Manutenção - Fluxo de Exportação).*
*&                                       |BUG Solto 167326.                                              *
*--------------------------------------------------------------------------------------------------------*

    CHECK i_docnum IS NOT INITIAL OR i_data IS NOT INITIAL.

    SELECT docnumflote AS docnum_flote, docnumeprod AS docnum_eprod, idvinc AS id_vinc, docnumref AS docnum_ref,
           chavenfe AS chave_nfe, qtdvinc AS qtd_vinc, vinculadaxml AS vinculada_xml, vincvirtual AS vinc_virtual, uscriacao AS us_criacao,
**<<<------"167326 - NMS - INI------>>>
*           dtcriacao AS dt_criacao, hrcriacao AS hr_criacao, cancel AS cancel, uscancel AS us_cancel, dtcancel AS dt_cancel, hrcancel AS hr_cancel
           dtcriacao AS dt_criacao, hrcriacao AS hr_criacao, manual AS manual, cancel AS cancel, uscancel AS us_cancel, dtcancel AS dt_cancel, hrcancel AS hr_cancel
**<<<------"167326 - NMS - FIM------>>>
      FROM zisd_vinculo_flote
      INTO CORRESPONDING FIELDS OF TABLE @r_vincflote
      WHERE docnumflote IN @i_docnum
        AND dtcriacao   IN @i_data.
**<<<------"165575 - NMS - INI------>>>
* Verifica se o parâmetro Nº documento (I_DOCNUM) não está vazio para não gerar duplicidade de registros.
    CHECK i_docnum IS NOT INITIAL.
**<<<------"165575 - NMS - FIM------>>>
    SELECT docnumflote AS docnum_flote, docnumeprod AS docnum_eprod, idvinc AS id_vinc, docnumref AS docnum_ref,
           chavenfe AS chave_nfe, qtdvinc AS qtd_vinc, vinculadaxml AS vinculada_xml, vincvirtual AS vinc_virtual, uscriacao AS us_criacao,
**<<<------"167326 - NMS - INI------>>>
*           dtcriacao AS dt_criacao, hrcriacao AS hr_criacao, cancel AS cancel, uscancel AS us_cancel, dtcancel AS dt_cancel, hrcancel AS hr_cancel
           dtcriacao AS dt_criacao, hrcriacao AS hr_criacao, manual AS manual, cancel AS cancel, uscancel AS us_cancel, dtcancel AS dt_cancel, hrcancel AS hr_cancel
**<<<------"167326 - NMS - FIM------>>
      FROM zisd_vinculo_flote
      APPENDING CORRESPONDING FIELDS OF TABLE @r_vincflote
      WHERE docnumeprod IN @i_docnum
        AND dtcriacao   IN @i_data.

  ENDMETHOD.


  METHOD get_cfop.

*"// CFOP Formação de lote
    IF i_formacao_lote IS NOT INITIAL.
      SELECT 'I'  AS sign,
             'EQ' AS option,
             cfop AS low
        FROM zsdt0354
        APPENDING TABLE @r_cfop
      WHERE excluido EQ @space.
    ENDIF.

*"// CFOP Entrada por Transferência
    IF i_transferencia IS NOT INITIAL.
      SELECT 'I'  AS sign,
             'EQ' AS option,
             cfop AS low
        FROM zsdt0362
        APPENDING TABLE @r_cfop
      WHERE excluido EQ @abap_false.
    ENDIF.

*"// CFOP Comercialização
    IF i_comercializacao IS NOT INITIAL.
      SELECT 'I'  AS sign,
             'EQ' AS option,
             cfop AS low
        FROM zsdt0352
        APPENDING TABLE @r_cfop
      WHERE excluido EQ @abap_false.
    ENDIF.

*"// CFOP Fins Especifico
    IF i_fim_especifico IS NOT INITIAL.
      SELECT 'I' AS sign,
             'EQ' AS option,
             cfop AS low
        FROM zsdt0353
        APPENDING TABLE @r_cfop
      WHERE excluido EQ @abap_false.
    ENDIF.

*"// CFOP Retorno de Formação de Lote
    IF i_retorno_formacao_lote IS NOT INITIAL.
      SELECT 'I' AS sign,
             'EQ' AS option,
             cfop AS low
        FROM zsdt0355
        APPENDING TABLE @r_cfop
      WHERE excluido EQ @abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD valida_registro_fila.


    IF c_zsdtprod_flote_t[] IS NOT INITIAL.
      SELECT docnum, cancel, regio, bukrs, branch
        FROM j_1bnfdoc INTO TABLE @DATA(lit_doc)
         FOR ALL ENTRIES IN @c_zsdtprod_flote_t
       WHERE docnum EQ @c_zsdtprod_flote_t-docnum.
    ENDIF.

    IF c_zsdtflote_flote_t[] IS NOT INITIAL.
      SELECT docnum, cancel, regio, bukrs, branch
        FROM j_1bnfdoc APPENDING TABLE @lit_doc
         FOR ALL ENTRIES IN @c_zsdtflote_flote_t
       WHERE docnum EQ @c_zsdtflote_flote_t-docnum.
    ENDIF.


    LOOP AT c_zsdtprod_flote_t ASSIGNING FIELD-SYMBOL(<fs_prod_lote>).
      READ TABLE lit_doc INTO DATA(lwa_doc) WITH KEY docnum = <fs_prod_lote>-docnum.
      IF sy-subrc EQ 0 AND lwa_doc-cancel EQ abap_true.
        CLEAR: <fs_prod_lote>-docnum.
      ENDIF.
    ENDLOOP.

    LOOP AT c_zsdtflote_flote_t ASSIGNING FIELD-SYMBOL(<fs_flote_flote>).
      READ TABLE lit_doc INTO lwa_doc WITH KEY docnum = <fs_flote_flote>-docnum.
      IF sy-subrc EQ 0 AND lwa_doc-cancel EQ abap_true.
        CLEAR: <fs_flote_flote>-docnum.
      ENDIF.
    ENDLOOP.

    DELETE  c_zsdtprod_flote_t  WHERE docnum IS INITIAL.
    DELETE  c_zsdtflote_flote_t WHERE docnum IS INITIAL.


  ENDMETHOD.


  METHOD vincula_nfe_saida_com_entrada.
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 05/02/2025|DEVK9A1XAW |NSEGATIN       | Ajuste do valor do retorno EUDR para consulta e na ordenação  *
*&                                       | da tabela de Notas de Produtor para Vincular (ZSDTPROD_FLOTE).*
*&                                       | Chamado 165575.                                               *
*--------------------------------------------------------------------------------------------------------*
*& 17/02/2025|DEVK9A1XAW |NSEGATIN       | Nota ded Produto Próprio (Produção interna) será considerado  *
*&                                       | no processo do vinculo de notas. Chamado 165835.              *
*--------------------------------------------------------------------------------------------------------*
*& 14/03/2025|DEVK9A1XAW |NSEGATIN       | Bloqueio/Autorização de processo 1x1 quando não há saldo      *
*&                                       | disponível a vincular. Chamado 168894.                        *
*--------------------------------------------------------------------------------------------------------*
*& 27/03/2025|DEVK9A1XAW |NSEGATIN       | Ajuste do campo de vinculo manual que não pode estar marcado  *
*&                                       | no processo automático. Chamado 167326.                       *
*--------------------------------------------------------------------------------------------------------*
*& 04/04/2025|DEVK9A1XAW |NSEGATIN       | Ajuste da chave de acesso de referência quando for registro de*
*&                                       | vinculo da FLOTE_FLOTE. Chamado 171563.                       *
*--------------------------------------------------------------------------------------------------------*
*& 11/04/2025|DEVK9A1XAW |NSEGATIN       | Implementação do campo Integração no processo de Bloqueio ou  *
*&                                       | Autorização do 1x1 se saldo disponível. Chamado 173808.       *
*--------------------------------------------------------------------------------------------------------*
*& 27/05/2025|DEVK9A2KPN |NSEGATIN       | Modificação e atualização de Tabelas transparentes do processo*
*&                                       | de vínculo por conta do Bloqueio ou Autorização do 1x1 sem    *
*&                                       | saldo disponível. Chamado 180725.                             *
*--------------------------------------------------------------------------------------------------------*

    "Classes
    DATA: zcl_util            TYPE REF TO zcl_util.

    "Internal Tables
    DATA: lt_vbrk             TYPE TABLE OF vbrk.
    DATA: wa_zsdtvinc_p_flote TYPE zsdtvinc_p_flote.

    CLEAR: lt_vbrk[].

    DATA: vl_chave_nfe       TYPE zde_chave_doc_e,
          vl_menge_saldo     TYPE j_1bnetqty,
          vl_saldo_vinc      TYPE j_1bnetqty,
          vl_qtd_vinc        TYPE j_1bnetqty,
          vl_user_form_lote  TYPE tvarvc-low,
          vl_tp_doc_fat      TYPE tvarvc-low,
          "v_vbeln            TYPE vbeln, "WPP - Code Review 25.11.24 --->>>>
          "v_vbelv            TYPE vbelv, "WPP - Code Review 25.11.24 --->>>>
          v_candat           TYPE sy-datum,
          _wl_ee_zgr_docs    TYPE zmmt_ee_zgr_docs,
          lv_eudr            TYPE zsdtprod_flote-eudr,
          lt_registros_fila  TYPE TABLE OF zsdtprod_flote,
          lt_zsdtprod_flote  TYPE TABLE OF zsdtprod_flote,
          lt_zsdtflote_flote TYPE zsdtflote_flote_t,
          ls_zsdtflote_flote TYPE zsdtflote_flote.
**<<<------"180725 - NMS - INI------>>>
* TIs de atualização de tabelas transparentes.
    DATA: tl_flote_flote_up  TYPE TABLE OF zsdtflote_flote,
          tl_prod_flote_up   TYPE TABLE OF zsdtprod_flote,
          tl_vinc_p_flote_up TYPE TABLE OF zsdtvinc_p_flote,
          tl_fl_flote_ref    TYPE TABLE OF zsdtfl_flote_ref.
**<<<------"180725 - NMS - FIM------>>>
    DATA: r_matkl          TYPE RANGE OF j_1bnflin-matkl.

    CONSTANTS: c_des_r_form_lote(29)  TYPE c VALUE 'Z_SD_DESATIVA_REGRA_FORM_LOTE',
               c_users_form_lote(24)  TYPE c VALUE 'Z_SD_USERS_FORMACAO_LOTE',
               c_form_lote_tp_fat(29) TYPE c VALUE 'Z_SD_FORM_LOTE_TP_FATURAMENTO'.

**<<<------"168894 - NMS - INI------>>>
* Solicita Aprovação/Rejeição Processo 1X1 Saldo indisponível
    sol_aprovacao_flote( EXPORTING i_atividade     = sy-abcde+2(1) "C - Consulta
                                   i_docnum_flote  = p_docnum
                         IMPORTING et_vinc_f_aprov = DATA(tl_vinc_f_aprov)
                                   et_zsdt0392     = DATA(tl_zsdt0392) "US #180203 - MMSILVA - 26.05.2025
                                  ).
    IF NOT tl_vinc_f_aprov IS INITIAL.
      SORT tl_vinc_f_aprov BY id_aprov DESCENDING docnum_flote bukrs werks matnr ASCENDING.
      READ TABLE tl_vinc_f_aprov INTO DATA(el_vinc_f_aprov) INDEX 1.
      READ TABLE tl_zsdt0392 INTO DATA(el_zsdt0392) INDEX 1. "US #180203 - MMSILVA - 26.05.2025

    ELSE.
      CLEAR el_vinc_f_aprov.

    ENDIF.
**<<<------"168894 - NMS - FIM------>>>
*** Se já existe vínculo, nada deve ser feito
    SELECT docnum_flote
      FROM zsdtvinc_p_flote
      INTO @DATA(lv_flote)
      UP TO 1 ROWS
      WHERE docnum_flote = @p_docnum.
    ENDSELECT.

    CHECK sy-subrc IS NOT INITIAL.

*** Se já existe nfenum, nada deve ser feito
    SELECT SINGLE nfenum
      FROM j_1bnfdoc
      INTO @DATA(lv_nfenum)
      WHERE docnum = @p_docnum.
    CHECK lv_nfenum IS INITIAL.

*** Busca grupos de materiais permitidos
    SELECT *
      FROM zsdt0356
      INTO TABLE @DATA(lt_zsdt0356)
      WHERE excluido EQ @space.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_zsdt0356 ASSIGNING FIELD-SYMBOL(<fs_zsdt0356>).
        APPEND INITIAL LINE TO r_matkl ASSIGNING FIELD-SYMBOL(<fs_matkl>).
        <fs_matkl>-sign   = 'I'.
        <fs_matkl>-option = 'EQ'.
        <fs_matkl>-low    = <fs_zsdt0356>-matkl.
      ENDLOOP.
    ENDIF.
* Verifica se a nota está cancelada. "<<<------"165835 - NMS------>>>
    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(_wl_active)
     WHERE docnum  = @p_docnum
       AND cancel  = @abap_false.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(wl_lin)
     WHERE docnum = @p_docnum
**<<<------"165835 - NMS - INI------>>>
*       AND matkl IN @r_matkl
*       AND ownpro EQ @space.
       AND matkl IN @r_matkl.
**<<<------"165835 - NMS - FIM------>>>
    CHECK ( sy-subrc EQ 0 ) AND ( wl_lin-refkey IS NOT INITIAL ).

    CALL METHOD zcl_eudr_utils=>check_doc_fiscal_eudr
      EXPORTING
        i_docnum = p_docnum
      RECEIVING
        r_eudr   = lv_eudr.
    IF lv_eudr EQ 'S'.
      lv_eudr = abap_true.
**<<<------"165575 - NMS - INI------>>>
    ELSE.
      lv_eudr = abap_false.
**<<<------"165575 - NMS - FIM------>>>
    ENDIF.


    " Seleciona Cabeçalho da nota fiscal
    SELECT SINGLE docnum, cancel, regio, bukrs, branch
      FROM j_1bnfdoc INTO @DATA(ls_j_1bnfdoc)
     WHERE docnum EQ @p_docnum.

    CHECK sy-subrc EQ 0.


    "WPP - Code Review 25.11.24 --->>>>
    DATA(wl_zsdt0001_s) = zcl_les_utils=>get_romaneio_documento_fiscal( i_docnum = p_docnum ).
    "  CHECK WL_ZSDT0001_S IS NOT INITIAL AND WL_ZSDT0001_S-TP_MOVIMENTO = 'S'.  "todas as formações de lote devem ser gerados processo 1X1 - Processo EUDR ?
**<<<------"173808 - NMS - INI------>>>
* Verifica se há integração.
    IF wl_zsdt0001_s IS INITIAL.
      DATA(vl_intergracao) = abap_off.

    ELSE.
      vl_intergracao = abap_on.

    ENDIF.
**<<<------"173808 - NMS - FIM------>>>
    zcl_les_utils=>check_doc_fiscal_rom_completo( EXPORTING i_docnum              = p_docnum
                                                  IMPORTING e_docnum_rom_completo = DATA(lit_docnum_rom_completo)
                                                            e_depara_romaneios    = DATA(lit_depara_romaneios) ).

    "IF line_exists( lit_docnum_rom_completo[ docnum = p_docnum ] ).
    "READ TABLE lit_depara_romaneios INTO DATA(lwa_depara_romaneio) INDEX 1.
    "DATA(lwa_doc_fiscal_rom_vinc) =  zcl_les_utils=>get_documento_fiscal_romaneio( i_ch_referencia = lwa_depara_romaneio-ch_ref_vinc ).
    "DATA(v_docnum_rom_e) = lwa_doc_fiscal_rom_vinc-docnum.

    DATA(r_ch_referencia_rom_ent) = VALUE rsis_t_range( FOR lwa_depara_rom IN lit_depara_romaneios (
                                                          sign    = 'I'
                                                          option  = 'EQ'
                                                          low     = lwa_depara_rom-ch_ref_vinc ) ).

    zcl_les_utils=>get_documento_fiscal_romaneio(
      EXPORTING
        i_ch_referencia_r           = r_ch_referencia_rom_ent
      IMPORTING
        e_docnums_fiscais_romaneios = DATA(lit_docnum_rom_entrada) ).

    IF lit_docnum_rom_entrada IS NOT INITIAL.

      DATA(wa_rom_entrada) =  lit_docnum_rom_entrada[ 1 ].
    ENDIF.



*    v_vbeln = wl_lin-refkey(10).
*
*    SELECT SINGLE a~vbelv
*      FROM vbfa AS a INTO v_vbelv
*     WHERE a~vbeln    EQ v_vbeln
*       AND a~vbtyp_n  IN ( 'M' , 'R' ) "Documento Material ou Fatura
*       AND a~vbtyp_v  EQ 'J'
*       AND NOT EXISTS ( SELECT *
*                         FROM vbfa AS b
*                        WHERE b~vbelv   = a~vbeln
*                          AND b~vbtyp_n = 'N' "estorno
*                       ).
*
*    IF ( sy-subrc EQ 0 ) AND ( v_vbelv IS NOT INITIAL ).
*
*      SELECT SINGLE *
*        FROM zsdt0001 INTO @DATA(wl_zsdt0001_s)
*       WHERE doc_rem       EQ @v_vbelv
*         AND tp_movimento  EQ 'S'.
*
*      IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0001_s-id_referencia IS NOT INITIAL ). "Romaneio Completo
*
*        DATA(v_nr_romaneio) = wl_zsdt0001_s-id_referencia.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = v_nr_romaneio
*          IMPORTING
*            output = v_nr_romaneio.
*
*        "Busca Romaneio Entrada
*        SELECT *
*          FROM zsdt0001 INTO TABLE @DATA(tg_zsdt0001_e)
*         WHERE bukrs        EQ @wl_zsdt0001_s-bukrs
*           AND branch       EQ @wl_zsdt0001_s-branch
*           AND nr_romaneio  EQ @v_nr_romaneio
*           AND nr_safra     EQ @wl_zsdt0001_s-nr_safra
*           AND tp_movimento EQ 'E'.
*
*        CHECK lines( tg_zsdt0001_e[] ) EQ 1.
*
*        READ TABLE tg_zsdt0001_e ASSIGNING FIELD-SYMBOL(<fs_zsdt0001_e>) INDEX 1.
*
*        CHECK <fs_zsdt0001_e>-ch_referencia IS NOT INITIAL.
*
*        "Se peso Romaneio Saída for igual o Peso Romaneio Entrada
*        CHECK ( wl_zsdt0001_s-peso_liq EQ <fs_zsdt0001_e>-peso_liq ).
*
*        SELECT SINGLE *
*          FROM zmmt_ee_zgr INTO @DATA(_wl_ee_zgr)
*         WHERE ch_referencia EQ @<fs_zsdt0001_e>-ch_referencia.
*
*        IF sy-subrc EQ 0.
*
*          SELECT SINGLE *
*            FROM zmmt_ee_zgr_docs AS de
*            INTO _wl_ee_zgr_docs
*           WHERE obj_key EQ _wl_ee_zgr-obj_key
*             AND EXISTS (  SELECT docnum
*                             FROM j_1bnfdoc AS dc
*                            WHERE dc~docnum EQ de~docnum
*                              AND candat    EQ v_candat
*                              AND cancel    EQ space ).
*
*          IF ( sy-subrc EQ 0 ) AND ( _wl_ee_zgr_docs-docnum IS NOT INITIAL ).
*            DATA(v_docnum_rom_e) = _wl_ee_zgr_docs-docnum.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.

    "CHECK wl_zsdt0001_s IS NOT INITIAL.
    "WPP - Code Review 25.11.24 <<<----

    " Seleciona Tipo de documento
    SELECT SINGLE doctyp
      FROM j_1bnfdoc INTO @DATA(vl_doctyp)
     WHERE docnum EQ @p_docnum.

    CHECK vl_doctyp EQ '1'.

    SELECT *
      FROM vbrk INTO TABLE lt_vbrk
     WHERE vbeln EQ wl_lin-refkey(10).

    "===========================================================================================
    " Vinculo entre a formação de lote, e quais notas de Compra para fim especifico foram utilizadas
    "===========================================================================================

*** Inicio - Rubenilson - 09.10.24 #154153

    " Verifica se regra de Formação de lote está ativa
    SELECT ativa_form_lote
      FROM zsdt0361
      UP TO 1 ROWS INTO @DATA(vl_des_r_form_lote).
    ENDSELECT.

    "Cadastro da UF Vínculo NF Ent. X Saída XML
    SELECT regio
      FROM zsdt0363 INTO TABLE @DATA(lt_uf)
      WHERE excluido EQ @space.
    IF sy-subrc IS INITIAL.
      SORT lt_uf BY regio.
    ENDIF.

    " Verifica se regra de Formação de lote está ativa pra usuário
    SELECT user_form_lote
      FROM zsdt0360
      INTO TABLE @DATA(lt_users_form_lote)
      WHERE excluido EQ @space.
    IF sy-subrc IS INITIAL.
      SORT lt_users_form_lote BY user_form_lote.
    ENDIF.

    " Seleciona tp doc de faturamento comercialização
    SELECT *
      FROM zsdt0357
      INTO TABLE @DATA(lt_zsdt0357)
      WHERE excluido EQ @space.
    IF sy-subrc IS INITIAL.
      SORT lt_zsdt0357 BY fkart.
    ENDIF.

    " Seleciona tp doc de faturamento industrizalização
    SELECT *
      FROM zsdt0358
      INTO TABLE @DATA(lt_zsdt0358)
      WHERE excluido EQ @space.
    IF sy-subrc IS INITIAL.
      SORT lt_zsdt0358 BY fkart.
    ENDIF.

*** Fim - Rubenilson - 09.10.24 #154153

    vl_user_form_lote = sy-uname.
    READ TABLE lt_users_form_lote WITH KEY user_form_lote = vl_user_form_lote TRANSPORTING NO FIELDS BINARY SEARCH.
    CHECK sy-subrc NE 0 AND vl_des_r_form_lote IS INITIAL.


    CREATE OBJECT zcl_util.
    READ TABLE lt_vbrk INTO DATA(wa_vbrk) INDEX 1.

    READ TABLE lt_zsdt0357 TRANSPORTING NO FIELDS
    WITH KEY fkart = wa_vbrk-fkart
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE lt_zsdt0358 TRANSPORTING NO FIELDS
      WITH KEY fkart = wa_vbrk-fkart
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        DATA(lv_compra_comer) = abap_true.
      ENDIF.
    ENDIF.

    CHECK sy-subrc EQ 0.

    CHECK wl_lin-werks IS NOT INITIAL AND wl_lin-matnr IS NOT INITIAL.


    IF lit_docnum_rom_entrada[] IS NOT INITIAL AND wa_rom_entrada-romaneio_completo IS NOT INITIAL. "Fluxo por romaneio completo

      " Seleciona Notas de Produtor para Vincular
      SELECT *
        FROM zsdtprod_flote INTO TABLE lt_zsdtprod_flote
         FOR ALL ENTRIES IN lit_docnum_rom_entrada
       WHERE docnum                 EQ lit_docnum_rom_entrada-docnum
         AND werks_real             EQ wl_lin-werks
         AND material               EQ wl_lin-matnr
         AND saldo_nao_disponivel   EQ abap_false
         AND romaneio_completo      EQ abap_true
         AND eudr                   EQ lv_eudr
         AND cancel                 EQ abap_false.

      IF sy-subrc IS INITIAL.
        SORT lt_zsdtprod_flote BY docnum.
      ENDIF.
    ELSEIF wl_zsdt0001_s-romaneio_completo IS INITIAL.
      "ELSE.

      " Seleciona Notas de Produtor para Vincular
      SELECT *
        FROM zsdtflote_flote
        INTO TABLE lt_zsdtflote_flote
        WHERE werks_real           EQ wl_lin-werks
          AND material             EQ wl_lin-matnr
          AND saldo_nao_disponivel EQ abap_false
          AND eudr                 EQ lv_eudr
          AND cancel               EQ abap_false.
      IF sy-subrc IS INITIAL.
        SORT lt_zsdtflote_flote BY compra_fim_es DESCENDING docnum ASCENDING data_emissao ASCENDING.
      ENDIF.

      " Seleciona Notas de Produtor para Vincular
      SELECT *
        FROM zsdtprod_flote INTO TABLE lt_zsdtprod_flote
        WHERE werks_real           EQ wl_lin-werks
          AND material             EQ wl_lin-matnr
          AND saldo_nao_disponivel EQ abap_false
          AND eudr                 EQ lv_eudr
          AND entrada_transf       EQ space
          AND romaneio_completo    EQ abap_false
          AND cancel               EQ abap_false.

      " Seleciona Notas de Produtor para Vincular
      SELECT *
        FROM zsdtprod_flote
        INTO TABLE @DATA(lt_prod_transf)
        WHERE werks_real           EQ @wl_lin-werks
          AND material             EQ @wl_lin-matnr
          AND saldo_nao_disponivel EQ @abap_false
          AND eudr                 EQ @lv_eudr
          AND entrada_transf       EQ @abap_true
          AND romaneio_completo    EQ @abap_false
          AND cancel               EQ @abap_false.

    ENDIF.

    IF lv_compra_comer IS NOT INITIAL.
      DELETE lt_zsdtprod_flote  WHERE compra_fim_es IS NOT INITIAL.
      DELETE lt_zsdtflote_flote WHERE compra_fim_es IS NOT INITIAL.
    ENDIF.

    zcl_im_cl_fluxo_exportacao=>valida_registro_fila( CHANGING c_zsdtprod_flote_t  = lt_zsdtprod_flote
                                                               c_zsdtflote_flote_t = lt_zsdtflote_flote ).
**<<<------"165835 - NMS - INI------>>>
*    IF lt_zsdtprod_flote[] IS INITIAL AND lt_zsdtflote_flote[] IS INITIAL.
    IF lt_zsdtprod_flote[] IS INITIAL AND lt_zsdtflote_flote[] IS INITIAL AND wl_lin-ownpro IS INITIAL AND el_vinc_f_aprov-status NE 'A'.
**<<<------"165835 - NMS - FIM------>>>
**<<<------"168894 - NMS - INI------>>>
*      CALL METHOD zcl_im_cl_fluxo_exportacao=>permissao_estoque_negativo
*        EXPORTING
*          matnr            = wl_lin-matnr
*          branch           = ls_j_1bnfdoc-branch
*        IMPORTING
*          estoque_negativo = DATA(lv_estoque_neg).
*      IF lv_estoque_neg IS INITIAL.
*        ROLLBACK WORK.
*        MESSAGE 'Não há notas disponíveis para vincular' TYPE 'E'.
*        RETURN.
*      ENDIF.
* Verifica o Status de Aprovação.
      CASE el_vinc_f_aprov-status.
        WHEN abap_off. "Em aprovação
* Vderifica se não há solicitação de aprovação.
          IF el_vinc_f_aprov IS INITIAL.
* Solicita Aprovação/Rejeição Processo 1X1 Saldo indisponível.
            sol_aprovacao_flote( EXPORTING i_atividade         = sy-abcde+8(1) "I - Inserção
                                           i_bukrs             = _wl_active-bukrs
                                           ie_bnflin           = wl_lin
                                           i_menge_saldo       = vl_menge_saldo
                                           i_integra           = vl_intergracao                    "<<<------"173808 - NMS------>>>
                                           i_romaneio_completo = wa_rom_entrada-romaneio_completo
                                           i_eudr              = lv_eudr
                                 IMPORTING et_vinc_f_aprov     = tl_vinc_f_aprov
                                          ).
* Envia e-mail de aprovação do Processo 1X1 Saldo indisponível.
            email_aprovacao( EXPORTING it_vinc_f_aprov = tl_vinc_f_aprov
                                       i_bukrs         = _wl_active-bukrs ).

          ELSE.
* Envia e-mail de aprovação do Processo 1X1 Saldo indisponível.
            email_aprovacao( EXPORTING it_vinc_f_aprov = tl_vinc_f_aprov
                                       i_bukrs         = _wl_active-bukrs ).
            MESSAGE  | Formação de lote aguardando aprovação da área responsável. ID aprovação { el_vinc_f_aprov-id_aprov } |  TYPE 'S'.

          ENDIF.

        WHEN sy-abcde+17(1). "R - Regeitado
* Solicita Aprovação/Rejeição Processo 1X1 Saldo indisponível.
          sol_aprovacao_flote( EXPORTING i_atividade         = sy-abcde+8(1) "I - Inserção
                                         i_bukrs             = el_vinc_f_aprov-bukrs
                                         ie_bnflin           = wl_lin
                                         i_menge_saldo       = vl_menge_saldo
                                         i_integra           = vl_intergracao                    "<<<------"173808 - NMS------>>>
                                         i_romaneio_completo = el_vinc_f_aprov-romaneio_completo
                                         i_eudr              = el_vinc_f_aprov-eudr
                               IMPORTING et_vinc_f_aprov     = tl_vinc_f_aprov
                                        ).
* Envia e-mail de aprovação do Processo 1X1 Saldo indisponível.
          email_aprovacao( EXPORTING it_vinc_f_aprov = tl_vinc_f_aprov
                                     i_bukrs         = _wl_active-bukrs ).

          MESSAGE  | Formação de lote aguardando aprovação da área responsável. ID aprovação { el_vinc_f_aprov-id_aprov } |  TYPE 'S'.

        WHEN OTHERS.
* Do nothing
      ENDCASE.
**<<<------"168894 - NMS - FIM------>>>
    ENDIF.
**<<<------"165575 - NMS - INI------>>>
*    SORT lt_zsdtprod_flote BY docnum.
    SORT lt_zsdtprod_flote BY data_emissao docnum.
**<<<------"165575 - NMS - FIM------>>>
*------------------------------------------------------------------------------------------------------------------------*
*   Seleção de Documentos de Entradas Bloqueados para vinculo
*------------------------------------------------------------------------------------------------------------------------*
    IF lt_zsdtprod_flote IS NOT INITIAL.
      SELECT *
        FROM zsdtprod_bloq INTO TABLE @DATA(lt_bloq)
       WHERE bukrs  EQ @ls_j_1bnfdoc-bukrs
         AND branch EQ @ls_j_1bnfdoc-branch
         AND status EQ 'A'.

      IF sy-subrc IS INITIAL.
        SORT lt_bloq BY forne_cnpj.

        DATA(lt_prod) = lt_zsdtprod_flote.
        SORT lt_prod BY docnum.
        DELETE ADJACENT DUPLICATES FROM lt_prod COMPARING docnum.

        SELECT docnum, cnpj_bupla
          FROM j_1bnfdoc INTO TABLE @DATA(lt_docs)
           FOR ALL ENTRIES IN @lt_prod
          WHERE docnum = @lt_prod-docnum.

        IF sy-subrc IS INITIAL.
          SORT lt_docs BY docnum.
        ENDIF.
      ENDIF.
    ENDIF.

*------------------------------------------------------------------------------------------------------------------------*
*-----------------------------------| "Ordem de Priorização de vinculação de notas |-------------------------------------*
*------------------------------------------------------------------------------------------------------------------------*
*  "Ordem de Priorização de vinculação de notas de entrada com NF de Saida que esta sendo emitida...
*    1 - Notas de Formação de lote Recusadas
*    2 - Notas de Entrada Produtor
*    3 - Notas de Entrda de Transferencia *
*------------------------------------------------------------------------------------------------------------------------*

    "-----------------------------------------------------------------------
    " Montagem registros Fila - Inicio
    "-----------------------------------------------------------------------
    IF lt_zsdtflote_flote IS NOT INITIAL.
      MOVE-CORRESPONDING lt_zsdtflote_flote[] TO lt_registros_fila[].

      DATA(lt_flote_aux) = lt_zsdtflote_flote.
      SORT lt_flote_aux BY docnum.
      DELETE ADJACENT DUPLICATES FROM lt_flote_aux COMPARING docnum.

      SELECT *
        FROM zsdtfl_flote_ref INTO TABLE @DATA(lt_flote_ref)
        FOR ALL ENTRIES IN @lt_flote_aux
        WHERE docnum_flote = @lt_flote_aux-docnum
          AND cancel EQ @abap_false
          AND saldo_disponivel > 0.
      IF sy-subrc IS INITIAL.
        SORT lt_flote_ref BY docnum_flote.
      ENDIF.
    ENDIF.

    SORT lt_zsdtprod_flote BY compra_fim_es DESCENDING docnum ASCENDING data_emissao ASCENDING .
    APPEND LINES OF lt_zsdtprod_flote TO lt_registros_fila.

    SORT lt_prod_transf BY docnum ASCENDING data_emissao ASCENDING entrada_transf ASCENDING.
    APPEND LINES OF lt_prod_transf TO lt_registros_fila.

    "-----------------------------------------------------------------------
    " Montagem registros Fila - Fim
    "-----------------------------------------------------------------------

    READ TABLE lt_uf TRANSPORTING NO FIELDS WITH KEY regio = ls_j_1bnfdoc-regio BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      DATA(lv_regio_vinc_nfe_xml) = abap_true.
    ELSE.
      CLEAR lv_regio_vinc_nfe_xml.
    ENDIF.

    SORT lt_zsdtflote_flote BY docnum itmnum.

    lt_prod = lt_registros_fila.
    SORT lt_prod BY docnum.
    DELETE ADJACENT DUPLICATES FROM lt_prod COMPARING docnum.

    SELECT *
      FROM zsdtvinc_p_flote INTO TABLE @DATA(lt_vinc)
      FOR ALL ENTRIES IN @lt_registros_fila
      WHERE docnum_eprod = @lt_registros_fila-docnum
        AND docnum_flote = @ls_j_1bnfdoc-docnum.
    IF sy-subrc IS INITIAL.
      SORT lt_vinc BY docnum_eprod docnum_flote id_vinc DESCENDING.
    ENDIF.

    LOOP AT lt_registros_fila INTO DATA(wa_registro_fila).

      IF sy-tabix EQ 1.
        vl_menge_saldo = wl_lin-menge.
      ENDIF.

      IF vl_menge_saldo IS INITIAL.
        EXIT.
      ENDIF.

      CLEAR: wa_zsdtvinc_p_flote.

*------------------------------------------------------------------------------------------------------------------------*
*     Checa se registro esta bloqueado para vinculação
*------------------------------------------------------------------------------------------------------------------------*
      READ TABLE lt_docs ASSIGNING FIELD-SYMBOL(<fs_docs>) WITH KEY docnum = wa_registro_fila-docnum BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_bloq ASSIGNING FIELD-SYMBOL(<fs_bloq>) WITH KEY forne_cnpj = <fs_docs>-cnpj_bupla BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.


      IF wa_registro_fila-saldo_disponivel GT 0.
**<<<------"180725 - NMS - INI------>>>
*        READ TABLE lt_zsdtflote_flote TRANSPORTING NO FIELDS WITH KEY docnum = wa_registro_fila-docnum
        READ TABLE lt_zsdtflote_flote INTO DATA(el_flote_flote_up) WITH KEY docnum = wa_registro_fila-docnum
**<<<------"180725 - NMS - FIM------>>>
                                                                            itmnum = wa_registro_fila-itmnum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
**<<<------"180725 - NMS - INI------>>>
*          UPDATE zsdtflote_flote SET cancel    = abap_true
*                                     us_cancel = sy-uname
*                                     dt_cancel = sy-datum
*                                     hr_cancel = sy-uzeit
*                               WHERE docnum = wa_registro_fila-docnum
*                                 AND itmnum = wa_registro_fila-itmnum
*                                 AND seq    = wa_registro_fila-seq.
          el_flote_flote_up-seq       = wa_registro_fila-seq.
          el_flote_flote_up-cancel    = abap_true.
          el_flote_flote_up-us_cancel = sy-uname.
          el_flote_flote_up-dt_cancel = sy-datum.
          el_flote_flote_up-hr_cancel = sy-uzeit.
          APPEND el_flote_flote_up TO tl_flote_flote_up.
          CLEAR el_flote_flote_up.

**<<<------"180725 - NMS - FIM------>>>
        ELSE.
**<<<------"180725 - NMS - INI------>>>
*          UPDATE zsdtprod_flote SET cancel    = abap_true
*                                    us_cancel = sy-uname
*                                    dt_cancel = sy-datum
*                                    hr_cancel = sy-uzeit
*                              WHERE docnum = wa_registro_fila-docnum
*                                AND itmnum = wa_registro_fila-itmnum
*                                AND werks  = wa_registro_fila-werks
*                                AND seq    = wa_registro_fila-seq.
          wa_registro_fila-cancel    = abap_true.
          wa_registro_fila-us_cancel = sy-uname.
          wa_registro_fila-dt_cancel = sy-datum.
          wa_registro_fila-hr_cancel = sy-uzeit.
          APPEND wa_registro_fila TO tl_prod_flote_up.
          CLEAR: wa_registro_fila-cancel, wa_registro_fila-us_cancel, wa_registro_fila-dt_cancel, wa_registro_fila-hr_cancel.
**<<<------"180725 - NMS - FIM------>>>
        ENDIF.
**<<<------"180725 - NMS - INI------>>>
*        READ TABLE lt_zsdtflote_flote TRANSPORTING NO FIELDS WITH KEY docnum = wa_registro_fila-docnum
        READ TABLE lt_zsdtflote_flote INTO el_flote_flote_up WITH KEY docnum = wa_registro_fila-docnum
**<<<------"180725 - NMS - FIM------>>>
                                                                      itmnum = wa_registro_fila-itmnum BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.

          DATA(lv_chave_nf) = zcl_util->get_chave_nfe( wa_registro_fila-docnum ).

          SELECT SINGLE cancel
            FROM zib_nfe_dist_ter
            INTO @DATA(lv_cancel)
            WHERE chave_nfe = @lv_chave_nf.
          IF sy-subrc IS NOT INITIAL .

            IF wa_registro_fila-saldo_disponivel = wa_registro_fila-qtd_nf.
**<<<------"180725 - NMS - INI------>>>
*              UPDATE zsdtprod_flote SET cancel = abap_true
*                                        us_cancel = sy-uname
*                                        dt_cancel = sy-datum
*                                        hr_cancel = sy-uzeit
*                                  WHERE docnum = wa_registro_fila-docnum.
*              UPDATE zsdtflote_flote SET cancel = abap_true
*                                         us_cancel = sy-uname
*                                         dt_cancel = sy-datum
*                                         hr_cancel = sy-uzeit
*                                   WHERE docnum = wa_registro_fila-docnum.
              wa_registro_fila-cancel    = abap_true.
              wa_registro_fila-us_cancel = sy-uname.
              wa_registro_fila-dt_cancel = sy-datum.
              wa_registro_fila-hr_cancel = sy-uzeit.
              APPEND wa_registro_fila TO tl_prod_flote_up.
              CLEAR: wa_registro_fila-cancel, wa_registro_fila-us_cancel, wa_registro_fila-dt_cancel, wa_registro_fila-hr_cancel.

              el_flote_flote_up-cancel    = abap_true.
              el_flote_flote_up-us_cancel = sy-uname.
              el_flote_flote_up-dt_cancel = sy-datum.
              el_flote_flote_up-hr_cancel = sy-uzeit.
              APPEND el_flote_flote_up TO tl_flote_flote_up.
              CLEAR el_flote_flote_up.
**<<<------"180725 - NMS - FIM------>>>
              CONTINUE.
            ENDIF.

          ELSEIF lv_cancel IS NOT INITIAL.

            CONTINUE.

          ENDIF.

        ENDIF.

        IF vl_menge_saldo GT wa_registro_fila-saldo_disponivel.

          vl_menge_saldo = vl_menge_saldo - wa_registro_fila-saldo_disponivel.
          vl_saldo_vinc  = wa_registro_fila-saldo_vinc + wa_registro_fila-saldo_disponivel.
          vl_qtd_vinc    = wa_registro_fila-saldo_disponivel.
          CLEAR wa_registro_fila-saldo_disponivel.
          wa_registro_fila-saldo_nao_disponivel = abap_true.

        ELSEIF vl_menge_saldo LT wa_registro_fila-saldo_disponivel.
          wa_registro_fila-saldo_disponivel = wa_registro_fila-saldo_disponivel - vl_menge_saldo.
          vl_saldo_vinc                      = wa_registro_fila-saldo_vinc  + vl_menge_saldo.
          vl_qtd_vinc = vl_menge_saldo.
          CLEAR vl_menge_saldo.
        ELSE.
          vl_menge_saldo = vl_menge_saldo - wa_registro_fila-saldo_disponivel.
**<<<------"172379 - NMS - INI------>>>
*            vl_saldo_vinc  = wa_registro_fila-saldo_disponivel.
*            vl_qtd_vinc    = vl_saldo_vinc.
          vl_saldo_vinc  = wa_registro_fila-saldo_vinc + wa_registro_fila-saldo_disponivel.
          vl_qtd_vinc    = wa_registro_fila-saldo_disponivel.
**<<<------"172379 - NMS - FIN------>>>
          CLEAR wa_registro_fila-saldo_disponivel.
          wa_registro_fila-saldo_nao_disponivel = abap_true.

        ENDIF.

        READ TABLE lt_vinc ASSIGNING FIELD-SYMBOL(<fs_vinc>) WITH KEY docnum_eprod = wa_registro_fila-docnum
                                                                      docnum_flote = wl_lin-docnum.
        IF sy-subrc IS INITIAL.
          wa_zsdtvinc_p_flote-id_vinc = <fs_vinc>-id_vinc + 1.
        ELSE.
          wa_zsdtvinc_p_flote-id_vinc = 1.
        ENDIF.

        wa_zsdtvinc_p_flote-mandt        = sy-mandt.
        wa_zsdtvinc_p_flote-docnum_flote = wl_lin-docnum.
        wa_zsdtvinc_p_flote-docnum_eprod = wa_registro_fila-docnum.
        wa_zsdtvinc_p_flote-qtd_vinc     = vl_qtd_vinc.
        wa_zsdtvinc_p_flote-us_criacao   = sy-uname.
        wa_zsdtvinc_p_flote-dt_criacao   = sy-datum.
        wa_zsdtvinc_p_flote-hr_criacao   = sy-uzeit.

        IF wa_registro_fila-compra_fim_es EQ abap_true AND lv_regio_vinc_nfe_xml IS NOT INITIAL.
          wa_zsdtvinc_p_flote-chave_nfe     = zcl_util->get_chave_nfe( wa_registro_fila-docnum ).
          wa_zsdtvinc_p_flote-vinculada_xml = abap_true.
        ELSE.
**<<<------"171563 - NMS - INI------>>>
          READ TABLE lt_zsdtflote_flote TRANSPORTING NO FIELDS WITH KEY docnum = wa_registro_fila-docnum.

          IF sy-subrc IS INITIAL.
            wa_zsdtvinc_p_flote-chave_nfe     = zcl_util->get_chave_nfe( wa_registro_fila-docnum ).
            wa_zsdtvinc_p_flote-vinculada_xml = abap_true.

          ELSE.
**<<<------"171563 - NMS - FIM------>>>
            CLEAR: wa_zsdtvinc_p_flote-chave_nfe, wa_zsdtvinc_p_flote-vinculada_xml.
**<<<------"171563 - NMS - INI------>>>
          ENDIF.
**<<<------"171563 - NMS - FIM------>>>
        ENDIF.

        wa_registro_fila-saldo_vinc     = vl_saldo_vinc.

        READ TABLE lt_zsdtflote_flote TRANSPORTING NO FIELDS WITH KEY docnum = wa_registro_fila-docnum
                                                                      itmnum = wa_registro_fila-itmnum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_zsdtvinc_p_flote-vinc_virtual = abap_true.

          MOVE-CORRESPONDING wa_registro_fila TO ls_zsdtflote_flote.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZSEQFLOTE'
            IMPORTING
              number                  = ls_zsdtflote_flote-seq
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.

          ls_zsdtflote_flote-us_criacao = sy-uname.
          ls_zsdtflote_flote-dt_criacao = sy-datum.
          ls_zsdtflote_flote-hr_criacao = sy-uzeit.
**<<<------"180725 - NMS - INI------>>>
*          MODIFY zsdtflote_flote FROM ls_zsdtflote_flote.
          APPEND ls_zsdtflote_flote TO tl_flote_flote_up.
**<<<------"180725 - NMS - FIM------>>>
        ELSE.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZSEQPROD'
            IMPORTING
              number                  = wa_registro_fila-seq
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
**<<<------"172379 - NMS - INI------>>>
          CLEAR wa_registro_fila-manual.
**<<<------"172379 - NMS - FIM------>>>
          wa_registro_fila-us_criacao = sy-uname.
          wa_registro_fila-dt_criacao = sy-datum.
          wa_registro_fila-hr_criacao = sy-uzeit.
**<<<------"180725 - NMS - INI------>>>
*          MODIFY zsdtprod_flote FROM wa_registro_fila.
          APPEND wa_registro_fila TO tl_prod_flote_up.
**<<<------"180725 - NMS - FIM------>>>
        ENDIF.
**<<<------"180725 - NMS - INI------>>>
*        MODIFY zsdtvinc_p_flote FROM wa_zsdtvinc_p_flote.
        APPEND wa_zsdtvinc_p_flote TO tl_vinc_p_flote_up.
**<<<------"180725 - NMS - FIM------>>>
        APPEND wa_zsdtvinc_p_flote TO lt_vinc.
      ELSE.
**<<<------"180725 - NMS - INI------>>>
*        READ TABLE lt_zsdtflote_flote TRANSPORTING NO FIELDS
        READ TABLE lt_zsdtflote_flote INTO el_flote_flote_up
**<<<------"180725 - NMS - FIM------>>>
        WITH KEY docnum = wa_registro_fila-docnum
                 itmnum = wa_registro_fila-itmnum
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
**<<<------"180725 - NMS - INI------>>>
*          UPDATE zsdtflote_flote SET cancel    = abap_true
*                                   us_cancel = sy-uname
*                                   dt_cancel = sy-datum
*                                   hr_cancel = sy-uzeit
*                             WHERE docnum = wa_registro_fila-docnum
*                               AND itmnum = wa_registro_fila-itmnum
*                               AND seq    = wa_registro_fila-seq.
          el_flote_flote_up-cancel    = abap_true.
          el_flote_flote_up-us_cancel = sy-uname.
          el_flote_flote_up-dt_cancel = sy-datum.
          el_flote_flote_up-hr_cancel = sy-uzeit.
          APPEND el_flote_flote_up TO tl_flote_flote_up.
          CLEAR el_flote_flote_up.
**<<<------"180725 - NMS - FIM------>>>
        ELSE.
**<<<------"180725 - NMS - INI------>>>
*          UPDATE zsdtprod_flote SET cancel    = abap_true
*                                    us_cancel = sy-uname
*                                    dt_cancel = sy-datum
*                                    hr_cancel = sy-uzeit
*                              WHERE docnum = wa_registro_fila-docnum
*                                AND itmnum = wa_registro_fila-itmnum
*                                AND werks  = wa_registro_fila-werks
*                                AND seq    = wa_registro_fila-seq.
          wa_registro_fila-cancel    = abap_true.
          wa_registro_fila-us_cancel = sy-uname.
          wa_registro_fila-dt_cancel = sy-datum.
          wa_registro_fila-hr_cancel = sy-uzeit.
          APPEND wa_registro_fila TO tl_prod_flote_up.
          CLEAR: wa_registro_fila-cancel, wa_registro_fila-us_cancel, wa_registro_fila-dt_cancel, wa_registro_fila-hr_cancel.
**<<<------"180725 - NMS - FIM------>>>
        ENDIF.

        IF vl_menge_saldo GT wa_registro_fila-qtd_nf.

          vl_menge_saldo = vl_menge_saldo - wa_registro_fila-qtd_nf.
          vl_saldo_vinc  = wa_registro_fila-qtd_nf.
          CLEAR wa_registro_fila-saldo_disponivel.
          wa_registro_fila-saldo_nao_disponivel = abap_true.

        ELSEIF vl_menge_saldo LT wa_registro_fila-qtd_nf.
          wa_registro_fila-saldo_disponivel = wa_registro_fila-qtd_nf - vl_menge_saldo.
          vl_saldo_vinc                      = vl_menge_saldo.
          CLEAR vl_menge_saldo.
        ELSE.
          vl_menge_saldo = vl_menge_saldo - wa_registro_fila-qtd_nf.
          vl_saldo_vinc  = wa_registro_fila-qtd_nf.
          CLEAR wa_registro_fila-saldo_disponivel.
          wa_registro_fila-saldo_nao_disponivel = abap_true.
        ENDIF.


        READ TABLE lt_vinc ASSIGNING <fs_vinc>
        WITH KEY docnum_eprod = wa_registro_fila-docnum
                 docnum_flote = wl_lin-docnum.
        IF sy-subrc IS INITIAL.
          wa_zsdtvinc_p_flote-id_vinc = <fs_vinc>-id_vinc + 1.
        ELSE.
          wa_zsdtvinc_p_flote-id_vinc = 1.
        ENDIF.

        wa_zsdtvinc_p_flote-mandt        = sy-mandt.
        wa_zsdtvinc_p_flote-docnum_flote = wl_lin-docnum.
        wa_zsdtvinc_p_flote-docnum_eprod = wa_registro_fila-docnum.
        wa_zsdtvinc_p_flote-qtd_vinc     = vl_saldo_vinc.
        wa_zsdtvinc_p_flote-us_criacao   = sy-uname.
        wa_zsdtvinc_p_flote-dt_criacao   = sy-datum.
        wa_zsdtvinc_p_flote-hr_criacao   = sy-uzeit.
        IF wa_registro_fila-compra_fim_es EQ abap_true AND lv_regio_vinc_nfe_xml IS NOT INITIAL.
          wa_zsdtvinc_p_flote-chave_nfe = zcl_util->get_chave_nfe( wa_registro_fila-docnum ).
          wa_zsdtvinc_p_flote-vinculada_xml = abap_true.
        ELSE.
          CLEAR: wa_zsdtvinc_p_flote-chave_nfe, wa_zsdtvinc_p_flote-vinculada_xml.
        ENDIF.

        wa_registro_fila-saldo_vinc     = vl_saldo_vinc.

        READ TABLE lt_zsdtflote_flote TRANSPORTING NO FIELDS
        WITH KEY docnum = wa_registro_fila-docnum
                 itmnum = wa_registro_fila-itmnum
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_zsdtvinc_p_flote-vinc_virtual = abap_true.

          MOVE-CORRESPONDING wa_registro_fila TO ls_zsdtflote_flote.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZSEQFLOTE'
            IMPORTING
              number                  = ls_zsdtflote_flote-seq
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.

          ls_zsdtflote_flote-us_criacao = sy-uname.
          ls_zsdtflote_flote-dt_criacao = sy-datum.
          ls_zsdtflote_flote-hr_criacao = sy-uzeit.
**<<<------"180725 - NMS - INI------>>>
*          MODIFY zsdtflote_flote FROM ls_zsdtflote_flote.
          APPEND ls_zsdtflote_flote TO tl_flote_flote_up.
**<<<------"180725 - NMS - FIM------>>>
        ELSE.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZSEQPROD'
            IMPORTING
              number                  = wa_registro_fila-seq
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
**<<<------"172379 - NMS - INI------>>>
          CLEAR wa_registro_fila-manual.
**<<<------"172379 - NMS - FIM------>>>
          wa_registro_fila-us_criacao = sy-uname.
          wa_registro_fila-dt_criacao = sy-datum.
          wa_registro_fila-hr_criacao = sy-uzeit.
**<<<------"180725 - NMS - INI------>>>
*          MODIFY zsdtprod_flote FROM wa_registro_fila.
          APPEND wa_registro_fila TO tl_prod_flote_up.
**<<<------"180725 - NMS - FIM------>>>
        ENDIF.
**<<<------"180725 - NMS - INI------>>>
*        MODIFY zsdtvinc_p_flote FROM wa_zsdtvinc_p_flote.
        APPEND wa_zsdtvinc_p_flote TO tl_vinc_p_flote_up.
**<<<------"180725 - NMS - FIM------>>>
        APPEND wa_zsdtvinc_p_flote TO lt_vinc.

      ENDIF.

      IF wa_zsdtvinc_p_flote-vinc_virtual IS NOT INITIAL.

        READ TABLE lt_flote_ref TRANSPORTING NO FIELDS
        WITH KEY docnum_flote = wa_registro_fila-docnum
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          DATA(lv_saldo_ref) = wa_zsdtvinc_p_flote-qtd_vinc.

          CLEAR wa_zsdtvinc_p_flote.

          LOOP AT lt_flote_ref ASSIGNING FIELD-SYMBOL(<fs_flote_ref>) FROM sy-tabix.
            IF wa_registro_fila-docnum <> <fs_flote_ref>-docnum_flote.
              EXIT.
            ENDIF.

            IF  lv_saldo_ref IS INITIAL.
              EXIT.
            ENDIF.
**<<<------"180725 - NMS - INI------>>>
*            UPDATE zsdtfl_flote_ref SET cancel = abap_true
*                                     us_cancel = sy-uname
*                                     dt_cancel = sy-datum
*                                     hr_cancel = sy-uzeit
*                               WHERE docnum_flote = <fs_flote_ref>-docnum_flote
*                                 AND docnum_eprod = <fs_flote_ref>-docnum_eprod
*                                 AND seq          = <fs_flote_ref>-seq.
            <fs_flote_ref>-cancel    = abap_true.
            <fs_flote_ref>-us_cancel = sy-uname.
            <fs_flote_ref>-dt_cancel = sy-datum.
            <fs_flote_ref>-hr_cancel = sy-uzeit.
            APPEND <fs_flote_ref> TO tl_fl_flote_ref.
            CLEAR: <fs_flote_ref>-cancel, <fs_flote_ref>-us_cancel, <fs_flote_ref>-dt_cancel, <fs_flote_ref>-hr_cancel.
**<<<------"180725 - NMS - FIM------>>>
            IF lv_saldo_ref GT <fs_flote_ref>-saldo_disponivel.

              lv_saldo_ref = lv_saldo_ref - <fs_flote_ref>-saldo_disponivel.
              <fs_flote_ref>-qtd_vinc  = <fs_flote_ref>-qtd_vinc + <fs_flote_ref>-saldo_disponivel.
              DATA(lv_saldo_vinc) = <fs_flote_ref>-saldo_disponivel.
              CLEAR <fs_flote_ref>-saldo_disponivel.

            ELSEIF lv_saldo_ref LT <fs_flote_ref>-saldo_disponivel.
              <fs_flote_ref>-saldo_disponivel = <fs_flote_ref>-saldo_disponivel - lv_saldo_ref.
              lv_saldo_vinc                   = lv_saldo_ref.
              <fs_flote_ref>-qtd_vinc  = <fs_flote_ref>-qtd_vinc + lv_saldo_ref.
              CLEAR lv_saldo_ref.
            ELSE.
              lv_saldo_vinc  = lv_saldo_ref.
              lv_saldo_ref = lv_saldo_ref - <fs_flote_ref>-saldo_disponivel.
              <fs_flote_ref>-qtd_vinc  = <fs_flote_ref>-qtd_vinc + <fs_flote_ref>-saldo_disponivel.
              CLEAR <fs_flote_ref>-saldo_disponivel.
            ENDIF.

            READ TABLE lt_vinc ASSIGNING <fs_vinc>
            WITH KEY docnum_eprod = <fs_flote_ref>-docnum_eprod
                     docnum_flote = wl_lin-docnum.
            IF sy-subrc IS INITIAL.
              wa_zsdtvinc_p_flote-id_vinc = <fs_vinc>-id_vinc + 1.
            ELSE.
              wa_zsdtvinc_p_flote-id_vinc = 1.
            ENDIF.

            wa_zsdtvinc_p_flote-mandt        = sy-mandt.
            wa_zsdtvinc_p_flote-docnum_flote = wl_lin-docnum.
            wa_zsdtvinc_p_flote-docnum_eprod = <fs_flote_ref>-docnum_eprod.
            wa_zsdtvinc_p_flote-docnum_ref   = wa_registro_fila-docnum.
            wa_zsdtvinc_p_flote-qtd_vinc     = lv_saldo_vinc.
            wa_zsdtvinc_p_flote-us_criacao   = sy-uname.
            wa_zsdtvinc_p_flote-dt_criacao   = sy-datum.
            wa_zsdtvinc_p_flote-hr_criacao   = sy-uzeit.
            IF wa_registro_fila-compra_fim_es EQ abap_true AND lv_regio_vinc_nfe_xml IS NOT INITIAL.
              wa_zsdtvinc_p_flote-chave_nfe = zcl_util->get_chave_nfe( wa_registro_fila-docnum ).
              wa_zsdtvinc_p_flote-vinculada_xml = abap_true.
            ELSE.
**<<<------"171563 - NMS - INI------>>>
              READ TABLE lt_zsdtflote_flote TRANSPORTING NO FIELDS WITH KEY docnum = wa_registro_fila-docnum.

              IF sy-subrc IS INITIAL.
                wa_zsdtvinc_p_flote-chave_nfe     = zcl_util->get_chave_nfe( wa_registro_fila-docnum ).
                wa_zsdtvinc_p_flote-vinculada_xml = abap_true.

              ELSE.
**<<<------"171563 - NMS - FIM------>>>
                CLEAR: wa_zsdtvinc_p_flote-chave_nfe, wa_zsdtvinc_p_flote-vinculada_xml.
**<<<------"171563 - NMS - INI------>>>
              ENDIF.
**<<<------"171563 - NMS - FIM------>>>
            ENDIF.


            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr             = '01'
                object                  = 'ZSEQFLREF'
              IMPORTING
                number                  = <fs_flote_ref>-seq
              EXCEPTIONS
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                OTHERS                  = 8.

            <fs_flote_ref>-us_criacao = sy-uname.
            <fs_flote_ref>-dt_criacao = sy-datum.
            <fs_flote_ref>-hr_criacao = sy-uzeit.
**<<<------"180725 - NMS - INI------>>>
*            MODIFY zsdtfl_flote_ref FROM <fs_flote_ref>.
*            MODIFY zsdtvinc_p_flote FROM wa_zsdtvinc_p_flote.
            APPEND <fs_flote_ref>      TO tl_fl_flote_ref.
            APPEND wa_zsdtvinc_p_flote TO tl_vinc_p_flote_up.
**<<<------"180725 - NMS - FIM------>>>
            APPEND wa_zsdtvinc_p_flote TO lt_vinc.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDLOOP.
**<<<------"165835 - NMS - INI------>>>
*    IF vl_menge_saldo > 0.
    IF vl_menge_saldo > 0 AND wl_lin-ownpro IS INITIAL AND el_vinc_f_aprov-status NE 'A'.
**<<<------"165835 - NMS - FIM------>>>
**<<<------"168894 - NMS - INI------>>>
*      CALL METHOD zcl_im_cl_fluxo_exportacao=>permissao_estoque_negativo
*        EXPORTING
*          matnr            = wl_lin-matnr
*          branch           = ls_j_1bnfdoc-branch
*        IMPORTING
*          estoque_negativo = lv_estoque_neg.
*      IF lv_estoque_neg IS INITIAL.
*        ROLLBACK WORK.
*        MESSAGE 'Não há notas na fila com saldo e/ou centro/material não está configurado para estoque negativo!' TYPE 'E'.
*      ENDIF.
* Verifica o Status de Aprovação.
      CASE el_vinc_f_aprov-status.
        WHEN abap_off. "Em aprovação
* Vderifica se não há solicitação de aprovação.
          IF el_vinc_f_aprov IS INITIAL.
* Solicita Aprovação/Rejeição Processo 1X1 Saldo indisponível.
            sol_aprovacao_flote( EXPORTING i_atividade         = sy-abcde+8(1) "I - Inserção
                                           i_bukrs             = _wl_active-bukrs
                                           ie_bnflin           = wl_lin
*                                           i_menge_saldo       = vl_menge_saldo **<<<------"180725 - SMC - ----->>>
                                           i_menge_saldo       = vl_qtd_vinc "<<<------"180725 - SMC - ----->>>
                                           i_integra           = vl_intergracao                    "<<<------"173808 - NMS------>>>
                                           i_romaneio_completo = wa_rom_entrada-romaneio_completo
                                           i_eudr              = lv_eudr
                                 IMPORTING et_vinc_f_aprov     = tl_vinc_f_aprov
                                          ).
* Envia e-mail de aprovação do Processo 1X1 Saldo indisponível.
            email_aprovacao( EXPORTING it_vinc_f_aprov = tl_vinc_f_aprov
                                       i_bukrs         = _wl_active-bukrs ).

          ELSE.
* Envia e-mail de aprovação do Processo 1X1 Saldo indisponível.
            email_aprovacao( EXPORTING it_vinc_f_aprov = tl_vinc_f_aprov
                                       i_bukrs         = _wl_active-bukrs ).

          ENDIF.

        WHEN sy-abcde+17(1). "R - Regeitado
* Solicita Aprovação/Rejeição Processo 1X1 Saldo indisponível.
          sol_aprovacao_flote( EXPORTING i_atividade         = sy-abcde+8(1) "I - Inserção
                                         i_bukrs             = el_vinc_f_aprov-bukrs
                                         ie_bnflin           = wl_lin
*                                         i_menge_saldo       = vl_menge_saldo "<<<------"180725 - SMC - ----->>>
                                         i_menge_saldo       = vl_qtd_vinc "<<<------"180725 - SMC - ----->>>
                                         i_integra           = vl_intergracao                    "<<<------"173808 - NMS------>>>
                                         i_romaneio_completo = el_vinc_f_aprov-romaneio_completo
                                         i_eudr              = el_vinc_f_aprov-eudr
                               IMPORTING et_vinc_f_aprov     = tl_vinc_f_aprov
                                        ).
* Envia e-mail de aprovação do Processo 1X1 Saldo indisponível.
          email_aprovacao( EXPORTING it_vinc_f_aprov = tl_vinc_f_aprov
                                     i_bukrs         = _wl_active-bukrs ).

        WHEN OTHERS.
* Do nothing
      ENDCASE.
**<<<------"168894 - NMS - FIM------>>>
    ENDIF.
**<<<------"168894 - NMS - INI------>>>
* Solicita Aprovação/Rejeição Processo 1X1 Saldo indisponível
    sol_aprovacao_flote( EXPORTING i_atividade     = sy-abcde+2(1) "C - Consulta
                                   i_docnum_flote  = p_docnum
                         IMPORTING et_vinc_f_aprov = tl_vinc_f_aprov
                                  ).

    IF NOT tl_vinc_f_aprov IS INITIAL.
      SORT tl_vinc_f_aprov BY id_aprov DESCENDING docnum_flote bukrs werks matnr ASCENDING.
      READ TABLE tl_vinc_f_aprov INTO el_vinc_f_aprov INDEX 1.

      IF     el_vinc_f_aprov-status   IS INITIAL AND
         NOT el_vinc_f_aprov-id_aprov IS INITIAL.
        MESSAGE 'Formação de lote aguardando aprovação da área responsável.' TYPE 'E'.
        RETURN.

      ENDIF.

    ENDIF.
**<<<------"168894 - NMS - FIM------>>>
**<<<------"180725 - NMS - INI------>>>
* Atualiza as tabelas que sofreram alteração durante o processo.
    IF NOT tl_flote_flote_up[] IS INITIAL.
* Notas de formação de lote Recusada para Vincular
      MODIFY zsdtflote_flote FROM TABLE tl_flote_flote_up.

    ENDIF.

    IF NOT tl_prod_flote_up[] IS INITIAL.
* Notas de Produtor para Vincular
      MODIFY zsdtprod_flote FROM TABLE tl_prod_flote_up.

    ENDIF.

    IF NOT tl_vinc_p_flote_up[] IS INITIAL.
* Tabela de vinculo entre a formação de lote
      MODIFY zsdtvinc_p_flote FROM TABLE tl_vinc_p_flote_up.

    ENDIF.

    IF NOT tl_fl_flote_ref[] IS INITIAL.
* Tabela auxiliar - Controle do Saldo do Liberado pelo Retorno
      MODIFY zsdtfl_flote_ref FROM TABLE tl_fl_flote_ref.

    ENDIF.
**<<<------"180725 - NMS - FIM------>>>
  ENDMETHOD.


  METHOD email_aprovacao.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : EMAIL_APROVACAO                                                                      *
*& Chamado        : USER STORY 168894                                                                    *
*& Data           : 14/03/2025                                                                           *
*& Especificado   : Leonardo Portela                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 17/03/2025|DEVK9A1XAW |NSEGATIN       | Envia e-mail de aprovação do Processo 1X1 Saldo indisponível  *
*--------------------------------------------------------------------------------------------------------*

    DATA: tl_html            TYPE STANDARD TABLE OF solisti1,
          tl_mailrecipientes TYPE STANDARD TABLE OF somlrec90.

    DATA vl_subject TYPE sood-objdes.

*   US #180203 - MMSILVA - 26.05.2025 - Inicio
    SELECT * FROM zsdt0392 INTO TABLE @DATA(tl_zsdt0392).
    READ TABLE tl_zsdt0392 INTO DATA(el_zsdt0392) INDEX 1.
    CHECK el_zsdt0392-status EQ 'A'.
*   US #180203 - MMSILVA - 26.05.2025 - Fim

    SELECT * FROM zsdtvinc_aprov
      INTO TABLE @DATA(tl_vinc_aprov)
    WHERE bukrs EQ @i_bukrs
      AND ativo EQ @abap_on.

    LOOP AT it_vinc_f_aprov INTO DATA(el_vinc_f_aprov) WHERE status EQ i_status.
* Create persistent send request
      DATA(cll_send_request) = cl_bcs=>create_persistent( ).
* Título do e-mail
      vl_subject = 'Aprovação Formação de Lote 1X1'.
* Verifica o Status so processamento.
      CASE i_status.
        WHEN space.          "Vazio - Em aprovação
          DATA(vl_txt_statu) = 'para aprovação'.

        WHEN sy-abcde(1).    "A - Aprovado
          vl_txt_statu = 'aprovado'.

        WHEN sy-abcde+17(1). "R - Reprovado
          vl_txt_statu = 'reprovado'.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.
* Corpo do e-mail
      APPEND '<html>' TO tl_html.
      APPEND '<style>' TO tl_html.
      APPEND 'table, th, td {border:1px solid black;}' TO tl_html.
      APPEND '</style>' TO tl_html.
      APPEND '<body>' TO tl_html.
      APPEND '<br>' TO tl_html.
      APPEND |Formação de lote | && vl_txt_statu &&
             | - Docnum: | && |{ el_vinc_f_aprov-docnum_flote ALPHA = OUT }| &&
             |, usuário solicitante: | && el_vinc_f_aprov-us_criacao &&
             |, Centro: | && el_vinc_f_aprov-werks &&
             |, Material: | && |{ el_vinc_f_aprov-matnr ALPHA = OUT }| && |.|
          TO tl_html.
      APPEND '<br>' TO tl_html.
      APPEND '</table>' TO tl_html.
      APPEND '</body>' TO tl_html.
      APPEND '</html>' TO tl_html.
* Criação do Documento para o envio do e-mail.
      DATA(cll_document) = cl_document_bcs=>create_document( i_type     = 'HTM'             "Formato do e-mail
                                                              i_text    = tl_html           "Corpo do e-mail
                                                              i_subject = |{ vl_subject }|  "Título do e-mail
                                                            ).
* Instancia o envio do e-mail.
      cll_send_request->set_document( cll_document ).
* Busca e-mail do Solicitante da aprovação.
      SELECT SINGLE smtp_addr
        FROM usr21 AS a
        INNER JOIN adr6 AS b
         ON a~persnumber EQ b~persnumber AND
            a~addrnumber EQ b~addrnumber
        INTO @DATA(lv_solic_email)
      WHERE bname EQ @el_vinc_f_aprov-us_criacao.

      IF sy-subrc IS INITIAL.
        APPEND VALUE #( receiver = lv_solic_email rec_type = 'U' rec_date = '00000000' ) TO tl_mailrecipientes.

      ENDIF.
* Montando a lista de envio de e-amil para aprovador(es).
      LOOP AT tl_vinc_aprov INTO DATA(el_vinc_aprov).
        APPEND VALUE #( receiver = el_vinc_aprov-grp_email rec_type = 'U' rec_date = '00000000' ) TO tl_mailrecipientes.

      ENDLOOP.
* Elimina e-mails duplicados.
      SORT tl_mailrecipientes BY receiver.
      DELETE ADJACENT DUPLICATES FROM tl_mailrecipientes COMPARING receiver.

      LOOP AT tl_mailrecipientes ASSIGNING FIELD-SYMBOL(<fs_address>).
        TRY.
            DATA(cll_recipient) = cl_cam_address_bcs=>create_internet_address( CONV #( |{ <fs_address>-receiver CASE = LOWER }| ) ).
            cll_send_request->add_recipient( cll_recipient ).
          CATCH cx_address_bcs.

        ENDTRY.

      ENDLOOP.
* Envio do e-mail.
      DATA(lv_sent_to_all) = cll_send_request->send( i_with_error_screen = abap_on ).
* Efetivação dos dados.
      COMMIT WORK.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_textos_flxexp_nf_saida.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : GET_TEXTOS_FLXEXP_NF_SAIDA                                                           *
*& Chamado        : USER STORY 178644                                                                    *
*& Data           : 13/05/2025                                                                           *
*& Especificado   : Samuel Cabana                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 13/05/2025|DEVK9A2JYP |NSEGATIN       | Textos NF Saída - Fluxo Exportação. Dev. Inicial.             *
*--------------------------------------------------------------------------------------------------------*

    DATA: vl_cgc    TYPE char18,
          vl_nfenum TYPE j_1bnfnum9.

    SELECT a~docnum_flote, a~docnum_eprod, b~cgc, b~name1, c~nfenum
      FROM zsdtvinc_p_flote AS a
      INNER JOIN j_1bnfnad AS b
       ON a~docnum_eprod EQ b~docnum
      INNER JOIN j_1bnfdoc AS c
       ON b~docnum EQ c~docnum
      INTO TABLE @DATA(tl_nf_vinc)
    WHERE a~docnum_flote EQ @i_docnum
      AND c~cancel       EQ @abap_off.

    IF sy-subrc IS INITIAL.
      LOOP AT tl_nf_vinc INTO DATA(el_nf_vinc).
        WRITE el_nf_vinc-cgc USING EDIT MASK 'RR__.___.___/____-__' TO vl_cgc.
        vl_nfenum = |{ el_nf_vinc-nfenum ALPHA = OUT }|.

        IF r_textos IS INITIAL.
          r_textos = |CNPJ: { vl_cgc } Fornecedor: { el_nf_vinc-name1 } Número NFE: { vl_nfenum }|.

        ELSE.
          r_textos = |{ r_textos }, CNPJ: { vl_cgc } Fornecedor: { el_nf_vinc-name1 } Número NFE: { vl_nfenum }|.

        ENDIF.

        AT LAST.
          r_textos = |{ r_textos }.|.

        ENDAT.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD sol_aprovacao_flote.
*&-------------------------------------------------------------------------------------------------------*
*& Método         : SOL_APROVACAO_FLOTE                                         *
*& Chamado        : USER STORY 168894                                                                    *
*& Data           : 14/03/2025                                                                           *
*& Especificado   : Leonardo Portela                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 14/03/2025|DEVK9A1XAW |NSEGATIN       | Solicita Aprovação/Rejeição Processo 1X1 Saldo indisponível   *
*--------------------------------------------------------------------------------------------------------*
*& 11/04/2025|DEVK9A1XAW |NSEGATIN       | Implementação do campo Integração no processo de Bloqueio ou  *
*&                                       | Autorização do 1x1 se saldo disponível. Chamado 173808.       *
*--------------------------------------------------------------------------------------------------------*

    DATA: el_vinc_f_aprov TYPE zsdtvinc_f_aprov,
          el_zsdt0392     TYPE zsdt0392. "US #180203 - MMSILVA - 26.05.2025

    SELECT * FROM zsdt0392 INTO TABLE et_zsdt0392. "US #180203 - MMSILVA - 26.05.2025

    CASE i_atividade.
      WHEN sy-abcde+2(1). "C - Consulta
        IF i_docnum_flote IS INITIAL.
          MESSAGE 'Quando consulta, o Doc FL é obrigatório' TYPE 'E'.
          RETURN.

        ELSE.
          SELECT * FROM zsdtvinc_f_aprov
            INTO TABLE et_vinc_f_aprov
          WHERE docnum_flote EQ i_docnum_flote.

        ENDIF.

      WHEN sy-abcde+8(1). "I - Inserção
        IF ie_bnflin IS INITIAL OR
           i_bukrs   IS INITIAL.
          MESSAGE 'Partida individual da NF e Empresa são obrigatórios na atividade de inserção' TYPE 'E'.
          RETURN.

        ELSE.
* Gera o número sequencial do campo ID_APROV da tabela ZSDCTVINC_F_APROV.
          PERFORM zf_number_get_next IN PROGRAM zsdr0012 USING    '01'
                                                                  'ZSEQAPROV'
                                                         CHANGING el_vinc_f_aprov-id_aprov.
          el_vinc_f_aprov-docnum_flote       = ie_bnflin-docnum.
          el_vinc_f_aprov-bukrs              = i_bukrs.
          el_vinc_f_aprov-werks              = ie_bnflin-werks.
          el_vinc_f_aprov-matnr              = ie_bnflin-matnr.
          el_vinc_f_aprov-qtd_flote          = ie_bnflin-menge.
          el_vinc_f_aprov-qtd_disponivel     = i_menge_saldo.
          el_vinc_f_aprov-eudr               = i_eudr.
          el_vinc_f_aprov-romaneio_completo  = i_romaneio_completo.
          el_vinc_f_aprov-integra            = i_integra.           "**<<<------"173808 - NMS------>>>
          el_vinc_f_aprov-us_criacao         = sy-uname.
          el_vinc_f_aprov-dt_criacao         = sy-datlo.
          el_vinc_f_aprov-hr_criacao         = sy-timlo.

* ------> US #180203 - MMSILVA - 26.05.2025 - Inicio <------
          READ TABLE et_zsdt0392 INTO el_zsdt0392 INDEX 1.
          IF el_zsdt0392-status EQ 'D'.
            el_vinc_f_aprov-status       = 'A'.
            el_vinc_f_aprov-dt_aprovacao = sy-datum.
            el_vinc_f_aprov-hr_aprovacao = sy-uzeit.
            el_vinc_f_aprov-us_aprovador = el_zsdt0392-us_aprovador.
            el_vinc_f_aprov-motivo       = 'Aprovação Automática'.
          ENDIF.
* ------> US #180203 - MMSILVA - 26.05.2025 - Fim <------

          MODIFY zsdtvinc_f_aprov FROM el_vinc_f_aprov.
          APPEND el_vinc_f_aprov TO et_vinc_f_aprov.

        ENDIF.

      WHEN OTHERS.
        DATA(vl_text) = |'Tipo de processamento não definido=>' { i_atividade }. |.
        MESSAGE vl_text TYPE 'E'.
        RETURN.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
