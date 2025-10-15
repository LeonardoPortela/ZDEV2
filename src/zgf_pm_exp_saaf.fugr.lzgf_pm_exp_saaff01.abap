*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_EXP_SAAFF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_E  text
*      -->P_025    text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      <--P_ET_RETURN  text
*----------------------------------------------------------------------*
FORM zf_inserir_mensagem USING  u_type    TYPE bapiret2-type
                              u_number  TYPE bapiret2-number
                              u_par_1   TYPE any
                              u_par_2   TYPE any
                              u_par_3   TYPE any
                              u_par_4   TYPE any.


  ls_return-type       = u_type.
  ls_return-id         = lc_zppm001.
  ls_return-number     = u_number.
  ls_return-message_v1 = u_par_1.
  ls_return-message_v2 = u_par_2.
  ls_return-message_v3 = u_par_3.
  ls_return-message_v4 = u_par_4.

  IF NOT ls_return-number = 999.
*
    MESSAGE ID lc_zppm001
            TYPE u_type
            NUMBER u_number
            WITH u_par_1
                 u_par_2
                 u_par_3
                 u_par_4
            INTO ls_return-message.
  ENDIF.
*
*  LS_RETURN-FIELD = WA_INPUT-I_COD_TRANSA_SAAF.

*

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_03 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  CLEAR it_zval[].
  SELECT zval const
    FROM ztparam
    INTO CORRESPONDING FIELDS OF TABLE it_param
    WHERE param EQ lc_tp_obj
    AND abastec EQ lc_x.
  LOOP AT it_param.
    it_zval-zval = it_param-zval.
    APPEND it_zval.
  ENDLOOP.
*
  SELECT equnr herst herld typbz objnr INTO TABLE it_equi
    FROM equi FOR ALL ENTRIES IN it_zval
    WHERE eqtyp   = it_zval-zval.
  SORT it_equi ASCENDING BY s_cod_veiculo.
  IF NOT it_equi[] IS INITIAL.
    SELECT objnr key_num license_num fuel_pri
           fuel_sec card_num fleet_num INTO TABLE it_fleet
      FROM fleet FOR ALL ENTRIES IN it_equi
      WHERE objnr = it_equi-objnr.
    SORT it_fleet ASCENDING BY objnr.
*
    SELECT fluid_type matnr INTO TABLE it_t370_mat
         FROM t370fld_mat FOR ALL ENTRIES IN it_fleet
      WHERE fluid_type = it_fleet-s_desc_comb_principal
         OR fluid_type = it_fleet-s_desc_comb_secundario_1.
    SORT it_t370_mat ASCENDING BY fluid_type.
*
    REFRESH it_m_veic_saaf.
*
    LOOP AT it_equi.
      CLEAR it_m_veic_saaf.
*
      CALL FUNCTION 'BAPI_EQUI_GETDETAIL' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          equipment        = it_equi-s_cod_veiculo
        IMPORTING
          data_general_exp = wa_data_gen.
*
      it_m_veic_saaf-s_descricao  = wa_data_gen-descript.
      it_m_veic_saaf-s_desc_reduzida  = wa_data_gen-descript.
* com Chave completa - tabela de Local de negócios
      SELECT adrnr INTO lv_adrnr
          FROM j_1bbranch
          WHERE bukrs  = wa_data_gen-comp_code    "com Chave primária
            AND branch = wa_data_gen-maintplant.  "com Chave primária
      ENDSELECT.
* com Chave primária - tabela de endereços
      SELECT city1 INTO it_m_veic_saaf-s_cidade_veiculo
        FROM adrc UP TO 1 ROWS
        WHERE addrnumber = lv_adrnr.
      ENDSELECT.
*
      READ TABLE it_fleet WITH KEY objnr = it_equi-objnr
         BINARY SEARCH.
      it_m_veic_saaf-s_cod_veiculo    = it_equi-s_cod_veiculo.
      it_m_veic_saaf-s_modelo_veiculo = it_equi-s_modelo_veiculo.
      it_m_veic_saaf-s_marca_veiculo  = it_equi-s_marca_veiculo.
      it_m_veic_saaf-i_capac_tanque_1 = it_fleet-i_capac_tanque_1.
      it_m_veic_saaf-s_placa          = it_fleet-s_placa .
      it_m_veic_saaf-s_grupo_combustivel = lc_comb.

      lv_langu = sy-langu.
      CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
        EXPORTING
          equipment     = it_equi-s_cod_veiculo
          language      = lv_langu
        TABLES
          system_status = it_status
          user_status   = it_userst.
*
      it_m_veic_saaf-ib_veiculo_ativo = lc_0.     "Ativo
      LOOP AT it_status.
        IF it_status-status = lc_i0076 OR
           it_status-status = lc_i0320.
          it_m_veic_saaf-ib_veiculo_ativo = lc_1. "Inativo
        ENDIF.
      ENDLOOP.

      it_m_veic_saaf-s_desc_comb_principal = it_fleet-s_desc_comb_principal.
      READ TABLE it_t370_mat WITH KEY fluid_type = it_fleet-s_desc_comb_principal
           BINARY SEARCH.
      IF sy-subrc IS INITIAL.
*---> 09/06/2023 - Migração S4 - JS
*       IT_M_VEIC_SAAF-S_COD_COMB_PRINCIPAL = IT_T370_MAT-MATNR.
        it_m_veic_saaf-s_cod_comb_principal = CONV #( it_t370_mat-matnr ).
*<--- 09/06/2023 - Migração S4 - JS
      ENDIF.

      it_m_veic_saaf-s_desc_comb_secundario_1 = it_fleet-s_desc_comb_secundario_1.
      READ TABLE it_t370_mat WITH KEY fluid_type = it_m_veic_saaf-s_desc_comb_secundario_1
           BINARY SEARCH.
      IF sy-subrc IS INITIAL.
*---> 09/06/2023 - Migração S4 - JS
*       IT_M_VEIC_SAAF-S_COD_COMB_SECUNDARIO_1 = IT_T370_MAT-MATNR.
        it_m_veic_saaf-s_cod_comb_secundario_1 = CONV #( it_t370_mat-matnr ).
*<--- 09/06/2023 - Migração S4 - JS
      ENDIF.
      it_m_veic_saaf-s_pais_veiculo   = it_equi-s_pais_veiculo.
      it_m_veic_saaf-s_div_1          = it_fleet-s_div_1.
      it_m_veic_saaf-s_div_2          = it_fleet-s_div_2.
*
      lv_x = lc_x.
      lv_002 = lc_002.
      lv_object =  it_equi-s_cod_veiculo.
      CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
        EXPORTING
          classtext          = lv_x
          classtype          = lc_002
          features           = lv_x
          language           = sy-langu
          object             = lv_object
          key_date           = sy-datum
        TABLES
          t_class            = it_class
          t_objectdata       = it_objectdata
        EXCEPTIONS
          no_classification  = 1
          no_classtypes      = 2
          invalid_class_type = 3
          OTHERS             = 4.
      IF sy-subrc IS INITIAL.
        READ TABLE it_class INDEX 1.
        it_m_veic_saaf-s_plano_veiculo = it_class-class.
      ENDIF.
*
      CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
        EXPORTING
          i_equnr   = it_equi-s_cod_veiculo
        TABLES
          et_diimpt = it_diimpt.
      IF NOT it_diimpt[] IS INITIAL.
* se encontrou o Point, busca a KM
        CLEAR lv_point.
        READ TABLE it_diimpt INTO wa_diimpt WITH KEY psort = lc_horim.
        IF sy-subrc IS INITIAL.
          lv_point = wa_diimpt-point.
        ELSE.
          READ TABLE it_diimpt INTO wa_diimpt WITH KEY psort = lc_odome.
          IF sy-subrc IS INITIAL.
            lv_point = wa_diimpt-point.
          ENDIF.
        ENDIF.
*
        IF NOT lv_point IS INITIAL.
          CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST_BUF'
            EXPORTING
              point   = lv_point
            IMPORTING
              imrg_wa = it_imrg_wa
                        EXCEPTIONS
                        imrg_not_found.
          IF sy-subrc IS INITIAL.
* Converte para KM
            CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
              EXPORTING
                char_unit       = it_imrg_wa-recdu
                fltp_value_si   = it_imrg_wa-cntrr
                indicator_value = lc_x
                masc_symbol     = ' '
              IMPORTING
                char_value      = lv_km_real_ch
              EXCEPTIONS
                no_unit_given   = 1
                unit_not_found  = 2
                OTHERS          = 3.
            IF sy-subrc IS INITIAL.
              TRANSLATE lv_km_real_ch USING '.,'.
* Esta é a quilometragem atual
              IF wa_diimpt-psort = lc_horim.
                it_m_veic_saaf-f_horimetro_inicial = lv_km_real_ch.
              ELSE.
                it_m_veic_saaf-f_km_inicial = lv_km_real_ch.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
* adiciona na View
      APPEND it_m_veic_saaf.
    ENDLOOP.
*
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*    ztpm_exp_p_saaf-tabela     = lc_ztpm_m_veic_saaf.
*    ztpm_exp_p_saaf-codigo     = it_equi-s_cod_veiculo.
*    ztpm_exp_p_saaf-timestamp  = lv_times.
*    MODIFY ztpm_exp_p_saaf.
  ENDIF.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_NOVOS_TABELA_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_novos_tabela_03 .
* Data a partir da qual haverá o processamento
  MOVE it_exp_p_saaf-timestamp TO lv_timess.
  CONDENSE lv_timess.
  MOVE lv_timess(8) TO lv_data.
  MOVE lv_timess+8(6) TO lv_time.
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
*
* Seleciona somente histórico dos campos HERST HERLD TYPBZ
* Equi
  it_fname-fname = lc_herst.
  APPEND it_fname.
  it_fname-fname = lc_herld.
  APPEND it_fname.
  it_fname-fname = lc_typbz.
  APPEND it_fname.
* fleet
  it_fname-fname = lc_key_num.
  APPEND it_fname.
  it_fname-fname = lc_license_num.
  APPEND it_fname.
  it_fname-fname = lc_fuel_pri.
  APPEND it_fname.
  it_fname-fname = lc_fuel_sec.
  APPEND it_fname.
  it_fname-fname = lc_card_num.
  APPEND it_fname.
  it_fname-fname = lc_fleet_num.
  APPEND it_fname.
*
  SORT it_fname ASCENDING BY fname.

*  move IT_EXP_P_SAAF-timestamp+8(6) to lv_time.
* Selecionar os registros alterados a partir do último processamennto
  SELECT * INTO TABLE it_cdhdr
    FROM cdhdr
    WHERE objectclas EQ lc_equi
      AND udate      GE lv_data.
  LOOP AT it_cdhdr.
    IF it_cdhdr-udate = lv_data.
      IF it_cdhdr-utime > lv_time.
        CONTINUE.
      ELSE.
        DELETE it_cdhdr.
      ENDIF.
    ENDIF.
  ENDLOOP.
*
  IF NOT it_cdhdr[] IS INITIAL.
    SELECT * INTO TABLE it_cdpos
     FROM cdpos FOR ALL ENTRIES IN it_cdhdr
     WHERE objectclas EQ lc_equi
       AND objectid   EQ it_cdhdr-objectid
       AND changenr   EQ it_cdhdr-changenr.
*       AND TABNAME    EQ LC_EQUI.
* Reduzir a it_cdpos
    LOOP AT it_cdpos.
      READ TABLE it_fname WITH KEY fname = it_cdpos-fname
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CONTINUE.
      ELSE.
        DELETE it_cdpos.
      ENDIF.
    ENDLOOP.
* Selecionar CDHDR a partir da it_cdpos reduzida.
    REFRESH it_cdhdr.
    SELECT * INTO TABLE it_cdhdr
      FROM cdhdr FOR ALL ENTRIES IN it_cdpos
      WHERE objectclas EQ lc_equi
        AND objectid   EQ it_cdpos-objectid
        AND changenr   EQ it_cdpos-changenr.
*
    CLEAR it_m_veic_saaf.
*
    SORT it_cdpos ASCENDING BY objectclas objectid.
*
    REFRESH it_m_veic_saaf.
*
    LOOP AT it_cdpos.
*
      IF NOT it_m_veic_saaf-s_cod_veiculo =  it_cdpos-objectid.
        IF NOT it_m_veic_saaf-s_cod_veiculo IS INITIAL.
* adiciona na View
          APPEND it_m_veic_saaf.
          CLEAR: it_m_veic_saaf.
        ENDIF.
      ENDIF.
* só os campos que interessam
      it_m_veic_saaf-s_cod_veiculo =  it_cdpos-objectid.

      CASE it_cdpos-fname.
        WHEN lc_herst.
          it_m_veic_saaf-s_marca_veiculo = it_cdpos-value_new.
          CONTINUE.
        WHEN lc_herld.
          it_m_veic_saaf-s_pais_veiculo = it_cdpos-value_new.
          CONTINUE.
        WHEN lc_typbz.
          it_m_veic_saaf-s_modelo_veiculo = it_cdpos-value_new.
          CONTINUE.
        WHEN lc_key_num.
          it_m_veic_saaf-i_capac_tanque_1 = it_cdpos-value_new.
          CONTINUE.
        WHEN lc_license_num.
          it_m_veic_saaf-s_placa = it_cdpos-value_new.
          CONTINUE.
        WHEN lc_fuel_pri.
          it_m_veic_saaf-s_desc_comb_principal = it_cdpos-value_new.
          CONTINUE.
        WHEN lc_fuel_sec.
          it_m_veic_saaf-s_desc_comb_secundario_1 = it_cdpos-value_new.
          CONTINUE.
        WHEN lc_card_num.
          it_m_veic_saaf-s_div_1 = it_cdpos-value_new.
          CONTINUE.
        WHEN lc_fleet_num.
          it_m_veic_saaf-s_div_2 = it_cdpos-value_new.
          CONTINUE.
        WHEN OTHERS. DELETE it_cdpos.
      ENDCASE.
*
    ENDLOOP.
*
    IF it_m_veic_saaf-s_cod_veiculo =  it_cdpos-objectid.
      IF NOT it_m_veic_saaf-s_cod_veiculo IS INITIAL.
* adiciona na View
        APPEND it_m_veic_saaf.
        CLEAR: it_m_veic_saaf.
      ENDIF.
    ENDIF.
*
  ENDIF.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_veic_saaf.
  CLEAR ztpm_exp_p_saaf-codigo.
  ztpm_exp_p_saaf-timestamp  = lv_times.
  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_04 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  SELECT pernr cname cpf_nr departamento
         funcao gbdat
         INTO TABLE it_user
    FROM zhcmt0007.
*
  REFRESH it_m_user_saaf.
*
  LOOP AT it_user.
    CLEAR it_m_user_saaf.
    it_m_user_saaf-dt_data_nascimento = it_user-dt_data_nascimento.
    it_m_user_saaf-s_cpf = it_user-s_cpf.
    it_m_user_saaf-s_perfil = it_user-s_perfil.
    it_m_user_saaf-s_lotacao = it_user-s_lotacao.
    it_m_user_saaf-s_nome = it_user-s_nome.
    it_m_user_saaf-s_senha = '???????'.
    it_m_user_saaf-s_id_teclado = it_user-s_id_teclado.
    it_m_user_saaf-s_barcode = it_user-s_id_teclado.
    APPEND it_m_user_saaf.
  ENDLOOP.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_  _saaf.
*  ztpm_exp_p_saaf-codigo     = it_.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_05 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  SELECT * INTO TABLE it_prod
     FROM ztftpm_lubri.
  SORT it_prod ASCENDING BY code_mat.
*
  SELECT fluid_type type_text INTO TABLE it_t370
      FROM t370fld_t FOR ALL ENTRIES IN it_prod
     WHERE fluid_type = it_prod-code_mat
       AND lang_key = lc_pt.
  SORT it_t370 ASCENDING BY fluid_type.
*
  REFRESH it_m_prod_saaf.
*
  LOOP AT it_prod.
    CLEAR it_m_prod_saaf.
    READ TABLE it_t370 WITH KEY fluid_type = it_prod-code_mat
       BINARY SEARCH.
    it_m_prod_saaf-s_cod_produto = it_prod-conjunto.
    it_m_prod_saaf-s_descricao = it_t370-type_text.
    it_m_prod_saaf-s_descricao_reduzida = it_t370-fluid_type.
    APPEND it_m_prod_saaf.
  ENDLOOP.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_06 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  SELECT code kurztext INTO TABLE it_qpct
    FROM qpct
    WHERE katalogart 	= lc_s
      AND codegruppe = lc_fpn
      AND inaktiv NE lc_x.
*
  REFRESH it_b_plan_saaf.
*
  LOOP AT it_qpct.
    CLEAR it_b_plan_saaf.
    it_b_plan_saaf-s_cod_operacao = it_qpct-s_cod_operacao.
    it_b_plan_saaf-s_desc_operacao = it_qpct-s_desc_operacao.
    it_b_plan_saaf-s_desc_reduzida = it_qpct-s_desc_operacao.
    APPEND it_b_plan_saaf.
  ENDLOOP.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_09
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_09 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  SELECT tplnr pltxt INTO TABLE it_iflo
     FROM iflo
    WHERE spras = lc_pt
     AND fltyp = lc_c.
  SORT it_iflo ASCENDING BY s_cod_local.
*
  REFRESH it_loc_ab_saaf.
*
  LOOP AT it_iflo.
    CLEAR it_loc_ab_saaf.
*
    lv_langu = sy-langu.
    lv_tplnr = it_iflo-s_cod_local.
    CALL FUNCTION 'BAPI_FUNCLOC_GETSTATUS'
      EXPORTING
        functlocation = lv_tplnr
        language      = lv_langu
      TABLES
        system_status = it_status
        user_status   = it_status.
*
    READ TABLE it_status WITH KEY status = lc_i0076.  "Inativo
    IF sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.
*
    READ TABLE it_status WITH KEY status = lc_i0320.  "Inativo
    IF sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.
*
    it_loc_ab_saaf-s_cod_local = it_iflo-s_cod_local.
    it_loc_ab_saaf-s_desc_local = it_iflo-s_desc_local.
    it_loc_ab_saaf-ib_vincular_comboio = lc_0.
    APPEND it_loc_ab_saaf.
  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_10 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  CLEAR it_zval[].
  SELECT zval const
    FROM ztparam
    INTO CORRESPONDING FIELDS OF TABLE it_param
    WHERE param EQ lc_tp_obj
    AND abastec EQ lc_x.
  LOOP AT it_param.
    it_zval-zval = it_param-zval.
    APPEND it_zval.
  ENDLOOP.
*
  SELECT equnr eqart INTO TABLE it_equi_m
    FROM equi FOR ALL ENTRIES IN it_zval
    WHERE eqtyp   = it_zval-zval.
  SORT it_equi_m ASCENDING BY equnr.
*
  "Modificado AOENNING.
  SELECT * INTO TABLE it_vei_op_saaf
    FROM ztpm_vei_op_saaf FOR ALL ENTRIES IN it_equi_m
    WHERE eqart = it_equi_m-eqart.
  SORT it_vei_op_saaf ASCENDING BY eqart.
*
  REFRESH it_r_ve_op_saaf.
*
*  LOOP AT IT_EQUI_M.
*    CLEAR IT_R_VE_OP_SAAF.
*
*    READ TABLE IT_VEI_OP_SAAF WITH KEY EQART = IT_EQUI_M-EQART
*      BINARY SEARCH.
*    IF SY-SUBRC IS INITIAL.
*      IT_R_VE_OP_SAAF-S_COD_OPERACAO = IT_VEI_OP_SAAF-CODE.
*      IT_R_VE_OP_SAAF-S_COD_VEICULO = IT_EQUI_M-EQUNR.
*      APPEND IT_R_VE_OP_SAAF.
*    ELSE.
*      CONTINUE.
*    ENDIF.
*  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_15
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_15 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  SELECT m~matnr t~maktx t~maktg INTO TABLE it_locas
    FROM mara AS m INNER JOIN makt AS t ON
               m~matnr EQ t~matnr
    WHERE m~mtart = lc_ibau
      AND m~lvorm = ''.
  SORT it_locas ASCENDING BY matnr.
*
  REFRESH it_b_comp_saaf.
*
  LOOP AT it_locas.
    CLEAR it_b_comp_saaf.
*
    it_b_comp_saaf-s_cod_compartimento = it_locas-matnr.
    it_b_comp_saaf-s_desc_red = it_locas-maktx.
    it_b_comp_saaf-s_desc = it_locas-maktg.
    APPEND it_b_comp_saaf.
  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*  MODIFY ztpm_exp_p_saaf.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_16
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_16 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  SELECT code kurztext INTO TABLE it_qpct
    FROM qpct
    WHERE katalogart 	= lc_r
      AND codegruppe = lc_fmot
      AND inaktiv NE lc_x.

  SORT it_qpct ASCENDING BY s_cod_operacao.
*
  REFRESH it_b_caus_saaf.
*
  LOOP AT it_qpct.
    CLEAR it_b_caus_saaf.
*
    it_b_caus_saaf-s_cod_causa_manut = it_qpct-s_cod_operacao.
    it_b_caus_saaf-s_desc = it_qpct-s_desc_operacao.
    it_b_caus_saaf-ib_vincular_comboio = lc_0.
    APPEND it_b_caus_saaf.
  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_18
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_18 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  CLEAR it_zval[].
  SELECT zval const
    FROM ztparam
    INTO CORRESPONDING FIELDS OF TABLE it_param
    WHERE param EQ lc_tp_obj
    AND abastec EQ lc_x.
  LOOP AT it_param.
    it_zval-zval = it_param-zval.
    APPEND it_zval.
  ENDLOOP.
*
  SELECT equnr herst herld typbz objnr INTO TABLE it_equi
    FROM equi FOR ALL ENTRIES IN it_zval
    WHERE eqtyp   = it_zval-zval.
  SORT it_equi ASCENDING BY s_cod_veiculo.
*
  REFRESH it_c_veic_saaf.
*
  LOOP AT it_equi.
    CLEAR it_c_veic_saaf.
*
    REFRESH it_diimpt.
*
    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        i_equnr   = it_equi-s_cod_veiculo
      TABLES
        et_diimpt = it_diimpt.
    IF NOT it_diimpt[] IS INITIAL.
* se encontrou o Point,
      DELETE it_diimpt WHERE locas = ''.
      DELETE it_diimpt WHERE inact = lc_x.
      LOOP AT it_diimpt INTO wa_diimpt.
        IF wa_diimpt-mptyp = lc_f OR
           wa_diimpt-mptyp = lc_h.
*
          it_c_veic_saaf-s_cod_veiculo = it_equi-s_cod_veiculo.
          it_c_veic_saaf-s_cod_compartimento =  wa_diimpt-locas.
          APPEND it_c_veic_saaf.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
* Elimina duplocados
  SORT it_c_veic_saaf ASCENDING BY s_cod_veiculo s_cod_compartimento.
  DELETE ADJACENT DUPLICATES FROM it_c_veic_saaf.
* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_INSERIR_TODA_TABELA_19
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_inserir_toda_tabela_19 .
* Preservar data/hora do ínício da seleção.
  CONCATENATE sy-datum sy-uzeit INTO lv_times.
* Selecionar todos os registros para serem enviados
  CLEAR it_zval[].
  SELECT zval const
    FROM ztparam
    INTO CORRESPONDING FIELDS OF TABLE it_param
    WHERE param EQ lc_tp_obj
    AND abastec EQ lc_x.
  LOOP AT it_param.
    it_zval-zval = it_param-zval.
    APPEND it_zval.
  ENDLOOP.
*
  SELECT equnr herst herld typbz objnr INTO TABLE it_equi
    FROM equi FOR ALL ENTRIES IN it_zval
    WHERE eqtyp   = it_zval-zval.
  SORT it_equi ASCENDING BY s_cod_veiculo.
*
  REFRESH it_b_cent_saaf.
*
  LOOP AT it_equi.
    CLEAR it_b_cent_saaf.
    CALL FUNCTION 'BAPI_EQUI_GETDETAIL' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        equipment        = it_equi-s_cod_veiculo
      IMPORTING
        data_general_exp = wa_data_gen.
* Select dentro de Loop com chave completa
    SELECT c~kostl t~ltext t~ktext INTO TABLE it_csks
      FROM csks AS c INNER JOIN cskt AS t
                   ON c~kokrs EQ t~kokrs AND
                      c~kostl EQ t~kostl AND
                      c~datbi EQ t~datbi
      WHERE c~kokrs = lc_magi
       AND c~kostl = wa_data_gen-costcenter
        AND c~datbi => sy-datum
        AND c~bukrs = wa_data_gen-comp_code
        AND t~spras = lc_pt.
*
    LOOP AT it_csks.
      it_b_cent_saaf-s_cod_centro_custo = it_csks-kostl.
      it_b_cent_saaf-s_desc = it_csks-ltext.
      it_b_cent_saaf-s_desc_red = it_csks-ktext.
      APPEND it_b_cent_saaf.
    ENDLOOP.
  ENDLOOP.

* Atualizar a tabela de controle com último código e data da última varredura
* Atualizar tabela ZTPM_EXP_P_SAAF
*  ztpm_exp_p_saaf-tabela     = lc_ztpm_m_ _saaf.
*  ztpm_exp_p_saaf-codigo     = it_o.
*  ztpm_exp_p_saaf-timestamp  = lv_times.
*
ENDFORM.
