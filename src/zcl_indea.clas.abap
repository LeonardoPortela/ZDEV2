class ZCL_INDEA definition
  public
  final
  create public .

public section.

  class-data IT_ZSDT0132 type ZSDT0132_T .
  class-data IT_ZSDT0207 type ZSDT0207_T .
  class-data IT_SAIDA_01 type ZSDE0408_T .
  class-data WA_SAIDA_01 type ZSDE0408 .
  class-data WA_ZSDT0132 type ZSDT0132 .
  class-data WA_ZSDT0207 type ZSDT0207 .

  class-methods SELECAO_DADOS_ROTEIRO
    importing
      !I_KUNNR type ZDE_KUNNR_T optional
      !I_NRO_ROT type Z_NR_ROT optional
      !I_SEM_PROPRIEDADE type CHAR01 optional
    exporting
      !E_SAIDA type ZSDE0408_T
      !E_ZSDT0132 type ZSDT0132_T
      !E_ZSDT0207 type ZSDT0207_T .
  class-methods ATUALIZAR_TABELAS_INDEA
    importing
      !I_ESP type CHAR01 optional
      !I_CULT type CHAR01 optional
      !I_CAT type CHAR01 optional
      !I_TPEM type CHAR01 optional
      !I_PRODUT type CHAR01 optional
      !I_TPAPLI type CHAR01 optional
      !I_UNID type CHAR01 optional
      !I_PRAGA type CHAR01 optional
      !I_P_AUTO type CHAR01 optional
      !I_PROD type CHAR01 optional
      !I_PROPRI type CHAR01 optional
      !I_URE type CHAR01 optional
      !I_SALDO type CHAR01 optional
      !I_TODOS type CHAR01 optional
      !I_KUNNR type ZDE_KUNNR_T
      !I_ESPEC type ZDE_ID_ESPECIE_T optional
      !I_NO_POPUP type CHAR01 optional
    exporting
      !E_ERRO type CHAR01 .
  class-methods CHECK_PRODUTOR_CLIENTE
    importing
      !I_KUNNR type KUNNR
    returning
      value(R_ENCONTROU) type CHAR01 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INDEA IMPLEMENTATION.


  METHOD atualizar_tabelas_indea.

    DATA: it_rsparams TYPE TABLE OF rsparams,
          wa_rsparams TYPE rsparams.

    FREE: e_erro.

    CLEAR it_rsparams[].

    IF i_kunnr IS NOT INITIAL.
      SELECT *
        FROM kna1
        INTO TABLE @DATA(lt_kna1)
        WHERE kunnr IN @i_kunnr.

      IF sy-subrc IS INITIAL.
        SORT lt_kna1 BY kunnr.
        LOOP AT i_kunnr INTO DATA(_kunnr).

          READ TABLE lt_kna1 TRANSPORTING NO FIELDS
          WITH KEY kunnr = _kunnr-low
          BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            e_erro = abap_true.
            MESSAGE s024(sd) WITH TEXT-e01 _kunnr-low TEXT-e02 DISPLAY LIKE 'E'.
            RETURN.
          ELSE.
            wa_rsparams-selname = 'S_KUNNR'.
            wa_rsparams-kind    = 'S'.
            wa_rsparams-sign    = 'I'.
            wa_rsparams-option  = 'EQ'.
            wa_rsparams-low     = _kunnr-low.
            APPEND wa_rsparams TO it_rsparams.
            CLEAR wa_rsparams.
          ENDIF.
        ENDLOOP.
      ELSE.
        e_erro = abap_true.
        MESSAGE s024(sd) WITH TEXT-e03 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
    LOOP AT i_espec[] ASSIGNING FIELD-SYMBOL(<fs_espec>).
      wa_rsparams-selname = 'S_ESPEC'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = <fs_espec>-low.
      APPEND wa_rsparams TO it_rsparams.
      CLEAR wa_rsparams.
    ENDLOOP.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = 'Atualizando Tabelas Indea...'.

    IF i_no_popup = abap_true.
      wa_rsparams-selname = 'P_NOPOP'.
      wa_rsparams-kind    = 'P'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = i_no_popup.
      APPEND wa_rsparams TO it_rsparams.
      CLEAR wa_rsparams.
    ENDIF.

    wa_rsparams-selname = 'P_TABELA'.
    wa_rsparams-kind    = 'S'.
    wa_rsparams-sign    = 'I'.
    wa_rsparams-option  = 'EQ'.

    IF i_esp = abap_true.    "Especie
      wa_rsparams-low     = 'INDEA_CONSULTA_ESPECIE'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_cult  = abap_true.     "Cultivar
      wa_rsparams-low     = 'INDEA_CONSULTA_CULTIVAR'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_cat  = abap_true.     "Categoria Sementes
      wa_rsparams-low     = 'INDEA_CONSULTA_CAT_SEMENTES'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_tpem  = abap_true.   "Tipo Embalagam
      wa_rsparams-low     = 'INDEA_CONSULTA_SITUACAO_EMBALAGEM'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_produt  = abap_true.   "Produto Autorizado
      wa_rsparams-low     = 'INDEA_CONSULTA_PRODUTO'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_tpapli  = abap_true.  "Tipo Aplicação,
      wa_rsparams-low     = 'INDEA_CONSULTA_TIPO_APLICACAO'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_unid  = abap_true. "unidade de medida
      wa_rsparams-low     = 'INDEA_CONSULTA_UNIDADE_MEDIDA'.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-high    = ' '.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_praga  = abap_true.   "Praga
      wa_rsparams-low     = 'INDEA_CONSULTA_PRAGA'.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-high    = ' '.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_p_auto = abap_true.  "Pessoa autorizada
      wa_rsparams-low     = 'INDEA_CONSULTA_PESSOA_AUTORIAZADA'.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-high    = ' '.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_prod  = abap_true.  "Produtor
      wa_rsparams-low     = 'INDEA_CONSULTA_PRODUTOR'.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-high    = ' '.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_propri  = abap_true.  "Propriedade
      wa_rsparams-low     = 'INDEA_CONSULTA_PROPRIEDADE'.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-high    = ' '.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_ure      = abap_true.  "URE
      wa_rsparams-low     = 'INDEA_CONSULTA_URE'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF  i_saldo    = abap_true.  "Saldo Revenda
      wa_rsparams-low     = 'INDEA_CONSULTA_SALDO_REVENDA'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.

    ELSEIF i_todos  = abap_true.
      wa_rsparams-selname = 'P_TABELA'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = 'INDEA_CONSULTA_ESPECIE'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_CULTIVAR'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_CAT_SEMENTES'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_SITUACAO_EMBALAGEM'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_PRODUTO'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_TIPO_APLICACAO'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_UNIDADE_MEDIDA'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_PRAGA'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_PESSOA_AUTORIAZADA'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_PRODUTOR'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_PROPRIEDADE'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_URE'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      wa_rsparams-low     = 'INDEA_CONSULTA_SALDO_REVENDA'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.
      SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check_produtor_cliente.

    FREE: r_encontrou.

    SELECT SINGLE id_produtor
      INTO @DATA(_id_produtor)
      FROM zsdt0206
     WHERE kunnr = @i_kunnr.

    CHECK sy-subrc = 0.

    SELECT SINGLE id_propriedade
      INTO @DATA(_id_propriedade)
      FROM zsdt0207
     WHERE id_produtor = @_id_produtor.

    IF sy-subrc = 0.
      r_encontrou = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD selecao_dados_roteiro.

    DATA: lra_nro_rot TYPE RANGE OF zsdt0132-nr_rot.

    DATA: wl_name     TYPE thead-tdname,
          it_texto    TYPE STANDARD TABLE OF tline,
          wa_texto    TYPE tline,
          tl_texto    TYPE catsxt_longtext_itab,
          wl_texto    TYPE LINE OF catsxt_longtext_itab,
          gt_estilo01 TYPE lvc_t_styl.

    FREE: e_saida, it_zsdt0132, it_zsdt0207, e_zsdt0132, e_zsdt0207, it_saida_01.

    IF i_nro_rot IS NOT INITIAL.
      APPEND VALUE #(  sign = 'I' option = 'EQ' low = i_nro_rot ) TO lra_nro_rot.
    ENDIF.

    IF i_sem_propriedade = abap_off.
      SELECT *
        FROM zsdt0132 INTO TABLE it_zsdt0132
      WHERE ( id_propriedade <> '0' )
        AND nr_rot IN lra_nro_rot
        AND ( kunnr IN i_kunnr )
        AND   status = 'A'. "ATIVO
    ELSE.
      SELECT *
        FROM zsdt0132 INTO TABLE it_zsdt0132
      WHERE ( id_propriedade  = 0 )
        AND nr_rot IN lra_nro_rot
        AND ( kunnr IN i_kunnr )
        AND   status = 'A'. "ATIVO
    ENDIF.

    CHECK it_zsdt0132 IS NOT INITIAL.

    IF i_sem_propriedade = abap_off.
      DELETE it_zsdt0132 WHERE id_propriedade IS INITIAL.
    ENDIF.

    IF it_zsdt0132[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0207 INTO TABLE it_zsdt0207
         FOR ALL ENTRIES IN it_zsdt0132
       WHERE id_propriedade EQ  it_zsdt0132-id_propriedade.
    ENDIF.

    LOOP AT it_zsdt0132 INTO wa_zsdt0132.

      wa_saida_01-nr_rot     = wa_zsdt0132-nr_rot.
      wa_saida_01-rot_desc   = wa_zsdt0132-rot_desc.
      wa_saida_01-kunnr      = |{ wa_zsdt0132-kunnr ALPHA = IN }|.
      wa_saida_01-city1      = wa_zsdt0132-city1.
      wa_saida_01-uf         = wa_zsdt0132-uf.
      wa_saida_01-tel_number = wa_zsdt0132-tel_number.
      wa_saida_01-status = wa_zsdt0132-status.

      SELECT SINGLE *
        FROM kna1 INTO @DATA(wa_kna1)
      WHERE kunnr EQ  @wa_saida_01-kunnr .

      IF sy-subrc = 0.
        wa_saida_01-name1 = wa_kna1-name1.
      ENDIF.

      READ TABLE it_zsdt0207 INTO wa_zsdt0207 WITH KEY  id_propriedade = wa_zsdt0132-id_propriedade.
      IF sy-subrc = 0.
        IF wa_zsdt0207-inativo IS NOT INITIAL.
          wa_saida_01-color = 'C600'.
        ENDIF.

        wa_saida_01-id_propriedade   = wa_zsdt0207-id_propriedade.
        wa_saida_01-nome             = wa_zsdt0207-nome.
        wa_saida_01-via_acesso       = wa_zsdt0207-via_acesso.
        wa_saida_01-municipio        = wa_zsdt0207-municipio.
      ENDIF.

      IF wa_zsdt0207-via_acesso IS INITIAL.
        wa_saida_01-texto02    = '@1F@'.
      ELSE.
        wa_saida_01-texto02    = '@1E@'.
      ENDIF.

      wl_name =  wa_zsdt0132-nr_rot.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'ZROT'
          language                = sy-langu
          name                    = wl_name
          object                  = 'ZSDROTEIRO'
        TABLES
          lines                   = it_texto
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF it_texto IS INITIAL.
        wa_saida_01-texto01 = '@1F@'.
      ELSE.
        wa_saida_01-texto01 = '@1E@'.
      ENDIF.

      FREE wa_saida_01-celltab.
      gt_estilo01 =  VALUE #( ( fieldname = 'KUNNR'           style = cl_gui_alv_grid=>mc_style_disabled  )
                              ( fieldname = 'NR_ROT'          style = cl_gui_alv_grid=>mc_style_disabled  )
                              ( fieldname = 'ID_PROPRIEDADE'  style = cl_gui_alv_grid=>mc_style_disabled  ) ).
      INSERT LINES OF gt_estilo01 INTO TABLE wa_saida_01-celltab.

      APPEND wa_saida_01 TO it_saida_01.
      CLEAR: wa_saida_01, wa_zsdt0207, wa_zsdt0132.
    ENDLOOP.

    e_saida[]    = it_saida_01[].
    e_zsdt0132[] = it_zsdt0132[].
    e_zsdt0207[] = it_zsdt0207[].

  ENDMETHOD.
ENDCLASS.
