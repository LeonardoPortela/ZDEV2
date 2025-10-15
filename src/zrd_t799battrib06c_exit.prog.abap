*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0208_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_t799battrib06c_exit.

"Programa anterior: ZLESR0150

FORM f_exit_t799battrib06c_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zhcme0001 TYPE zhcme0001.

  CLEAR: wl_zhcme0001.

  wl_zhcme0001-hr_attrib_class = 'CL_EDUC_INST'.
  wl_zhcme0001-molga = '37'.

  SELECT SINGLE hr_attribute_txt
   FROM t799battrib01t INTO wl_zhcme0001-hr_attribute_txt
     WHERE hr_attribute EQ wl_zhcme0001-hr_attrib_class.

  MOVE-CORRESPONDING wl_zhcme0001 TO p_registro_manter.

ENDFORM.

FORM f_exit_t799battrib06c_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zhcme0001 TYPE zhcme0001.

  CLEAR: wl_zhcme0001.

  MOVE-CORRESPONDING p_registro_manter TO wl_zhcme0001.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-saknr
*    IMPORTING
*      output = wl_zlest0208-saknr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-kostl
*    IMPORTING
*      output = wl_zlest0208-kostl.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-matnr
*    IMPORTING
*      output = wl_zlest0208-matnr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = wl_zlest0208-matkl
*    IMPORTING
*      output = wl_zlest0208-matkl.
*
*  CLEAR: p_error.

ENDFORM.

FORM f_exit_t799battrib06c_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zhcme0001 TYPE zhcme0001.

  CLEAR: wl_zhcme0001.

  MOVE-CORRESPONDING p_registro_manter TO wl_zhcme0001.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-saknr
*    IMPORTING
*      output = wl_zlest0208-saknr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-kostl
*    IMPORTING
*      output = wl_zlest0208-kostl.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wl_zlest0208-matnr
*    IMPORTING
*      output = wl_zlest0208-matnr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = wl_zlest0208-matkl
*    IMPORTING
*      output = wl_zlest0208-matkl.
*
*
*
*
*  wl_zlest0208-zdt_atual = sy-datum.
*  wl_zlest0208-zhr_atual = sy-uzeit.
*  wl_zlest0208-usnam = sy-uname.

  MOVE-CORRESPONDING wl_zhcme0001 TO p_registro_manter.

ENDFORM.

FORM f_exit_t799battrib06c_0004 CHANGING p_saida TYPE any.

  DATA: t_fieldcat   TYPE slis_t_fieldcat_alv,
        ztransaction TYPE sy-tcode,
        wl_zhcme0001 TYPE zhcme0001.
  CLEAR: wl_zhcme0001, ztransaction.


  DATA: wl_zhcme0002     TYPE zhcme0002,
        wl_zhcme0002_aux TYPE zhcme0002.
  CLEAR: wl_zhcme0002, wl_zhcme0002_aux.


  MOVE-CORRESPONDING p_saida TO wl_zhcme0001.
  MOVE-CORRESPONDING p_saida TO wl_zhcme0002_aux.
  MOVE-CORRESPONDING p_saida TO wl_zhcme0002.
  wl_zhcme0002-hr_cvalue = '0001'.

  "Verifica se esta executando pela transação ZHCMT0012 OU ZHCMT0013.
  MOVE-CORRESPONDING wl_zhcme0002 TO p_saida.

  CLEAR: wl_zhcme0002.
  MOVE-CORRESPONDING p_saida TO wl_zhcme0002.


  IF wl_zhcme0002-hr_cvalue IS INITIAL.
    ztransaction = 'ZHCMT0012'.
  ELSE.
    ztransaction = 'ZHCMT0013'.
  ENDIF.

* Ini - RJF - CS2024000177 Ajuste em novos dados de instituição de ensino - APRENDIZ / ESTAGIÁRIO
DATA: lv_dattr type char10.

IMPORT lv_dattr TO lv_dattr FROM MEMORY ID 'LV_DATTR'.
IF lv_dattr IS NOT INITIAL.
  ztransaction = lv_dattr.
*  FREE MEMORY ID 'LV_DATTR'.
ENDIF.

*  IF sy-title EQ 'Criar Código Entidade Qual.'.
*    ztransaction = 'ZHCMT0016'.
*  ELSEIF sy-title EQ 'Criar Código Local Prática'.
*    ztransaction = 'ZHCMT0017'.
*  ENDIF.
* Fim - RJF - CS2024000177 Ajuste em novos dados de instituição de ensino - APRENDIZ / ESTAGIÁRIO

  CLEAR: p_saida.
  CASE ztransaction.
    WHEN 'ZHCMT0012'.
      IF wl_zhcme0001-hr_attrib_class EQ 'CL_EDUC_INST'.
        SELECT SINGLE hr_att_group_txt
        FROM t799battrib06ct INTO wl_zhcme0001-hr_att_group_txt
          WHERE hr_attrib_group EQ wl_zhcme0001-hr_attrib_group.

        SELECT SINGLE hr_attribute_txt
        FROM t799battrib01t INTO wl_zhcme0001-hr_attribute_txt
          WHERE hr_attribute EQ wl_zhcme0001-hr_attrib_class.
        MOVE-CORRESPONDING wl_zhcme0001 TO p_saida.
      ENDIF.

    WHEN 'ZHCMT0013'.
      IF wl_zhcme0002_aux-hr_attrib_class EQ 'CL_EDUCATION_ORG'.
        SELECT SINGLE hr_att_group_txt
        FROM t799battrib06ct INTO wl_zhcme0002_aux-hr_att_group_txt
          WHERE hr_attrib_group EQ wl_zhcme0002_aux-hr_attrib_group.

        SELECT SINGLE hr_attribute_txt
        FROM t799battrib01t INTO wl_zhcme0002_aux-hr_attribute_txt
          WHERE hr_attribute EQ wl_zhcme0002_aux-hr_attrib_class.
*        wl_zhcme0002_aux-hr_cvalue = '0001'.
        MOVE-CORRESPONDING wl_zhcme0002_aux TO p_saida.
      ENDIF.

    WHEN 'ZHCMT0016'.
      IF wl_zhcme0002_aux-hr_attrib_class EQ 'CL_QUALIFIER_ENT'.
        SELECT SINGLE hr_att_group_txt
        FROM t799battrib06ct INTO wl_zhcme0002_aux-hr_att_group_txt
          WHERE hr_attrib_group EQ wl_zhcme0002_aux-hr_attrib_group.

        SELECT SINGLE hr_attribute_txt
        FROM t799battrib01t INTO wl_zhcme0002_aux-hr_attribute_txt
          WHERE hr_attribute EQ wl_zhcme0002_aux-hr_attrib_class.
*        wl_zhcme0002_aux-hr_cvalue = '0001'.
        MOVE-CORRESPONDING wl_zhcme0002_aux TO p_saida.
      ENDIF.

    WHEN 'ZHCMT0017'.
      IF wl_zhcme0002_aux-hr_attrib_class EQ 'CL_COMP_ACTIVITY'.
        SELECT SINGLE hr_att_group_txt
        FROM t799battrib06ct INTO wl_zhcme0002_aux-hr_att_group_txt
          WHERE hr_attrib_group EQ wl_zhcme0002_aux-hr_attrib_group.

        SELECT SINGLE hr_attribute_txt
        FROM t799battrib01t INTO wl_zhcme0002_aux-hr_attribute_txt
          WHERE hr_attribute EQ wl_zhcme0002_aux-hr_attrib_class.
*        wl_zhcme0002_aux-hr_cvalue = '0001'.
        MOVE-CORRESPONDING wl_zhcme0002_aux TO p_saida.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.
ENDFORM.


FORM f_exit_t799battrib06c_0011 CHANGING  p_registro_manter TYPE any
                                          p_saida TYPE any.

  DATA: wl_zhcme0001       TYPE zhcme0001,
        wl_t799battrib06ct TYPE t799battrib06ct,
        wl_t799battrib03c  TYPE t799battrib03c. "RJF
  CLEAR: wl_zhcme0001, wl_t799battrib06ct.
  MOVE-CORRESPONDING p_saida TO wl_t799battrib06ct.
  MOVE-CORRESPONDING p_registro_manter TO wl_zhcme0001.

  wl_t799battrib06ct-spras = sy-langu.
  wl_t799battrib06ct-molga = wl_zhcme0001-molga.
  wl_t799battrib06ct-hr_attrib_group = wl_zhcme0001-hr_attrib_group.

  IF wl_t799battrib06ct IS NOT INITIAL.
    MODIFY  t799battrib06ct FROM wl_t799battrib06ct.
    COMMIT WORK.
  ENDIF.

* Ini - RJF - CS2024000177 Ajuste em novos dados de instituição de ensino - APRENDIZ / ESTAGIÁRIO
  IF ( wl_t799battrib06ct IS NOT INITIAL AND ( sy-title EQ 'Criar Código Entidade Qual.' OR sy-title EQ 'Criar Código Local Prática' ) ).

    wl_t799battrib03c-hr_attribute    = 'AT_CNPJ'.
    wl_t799battrib03c-molga           = wl_t799battrib06ct-molga.
    wl_t799battrib03c-hr_attrib_group = wl_t799battrib06ct-hr_attrib_group.
    wl_t799battrib03c-endda           = '99991231'.
    wl_t799battrib03c-begda           = '19000101'.
    wl_t799battrib03c-hr_cvalue       = wl_t799battrib03c-hr_attrib_group.

    MODIFY t799battrib03c FROM wl_t799battrib03c.
    COMMIT WORK AND WAIT.
  ENDIF.
* Fim - RJF - CS2024000177 Ajuste em novos dados de instituição de ensino - APRENDIZ / ESTAGIÁRIO

  CLEAR: p_saida.


ENDFORM.

FORM f_exit_t799battrib06c_0012 CHANGING  p_registro_manter TYPE any
                                          p_saida TYPE any.

  DATA: t_fieldcat   TYPE slis_t_fieldcat_alv,
        ztransaction TYPE sy-tcode,
        wl_zhcme0001 TYPE zhcme0001.
  CLEAR: wl_zhcme0001, ztransaction.


  DATA: wl_zhcme0002     TYPE zhcme0002,
        wl_zhcme0002_aux TYPE zhcme0002.
  CLEAR: wl_zhcme0002, wl_zhcme0002_aux.


  MOVE-CORRESPONDING p_saida TO wl_zhcme0001.
  MOVE-CORRESPONDING p_saida TO wl_zhcme0002_aux.
  MOVE-CORRESPONDING p_saida TO wl_zhcme0002.
  wl_zhcme0002-hr_cvalue = '0001'.

  "Verifica se esta executando pela transação ZHCMT0012 OU ZHCMT0013.
  MOVE-CORRESPONDING wl_zhcme0002 TO p_saida.

  CLEAR: wl_zhcme0002.
  MOVE-CORRESPONDING p_saida TO wl_zhcme0002.


  IF wl_zhcme0002-hr_cvalue IS INITIAL.
    ztransaction = 'ZHCMT0012'.
  ELSE.
    ztransaction = 'ZHCMT0013'.
  ENDIF.

* Ini - RJF - CS2024000177 Ajuste em novos dados de instituição de ensino - APRENDIZ / ESTAGIÁRIO
DATA: lv_dattr type char10.

IMPORT lv_dattr TO lv_dattr FROM MEMORY ID 'LV_DATTR'.
IF lv_dattr IS NOT INITIAL.
  ztransaction = lv_dattr.
*  FREE MEMORY ID 'LV_DATTR'.
ENDIF.

*  IF sy-title EQ 'Criar Código Entidade Qual.'.
*    ztransaction = 'ZHCMT0016'.
*  ELSEIF sy-title EQ 'Criar Código Local Prática'.
*    ztransaction = 'ZHCMT0017'.
*  ENDIF.
* Fim - RJF - CS2024000177 Ajuste em novos dados de instituição de ensino - APRENDIZ / ESTAGIÁRIO

  CLEAR: p_saida.
  CASE ztransaction.
    WHEN 'ZHCMT0012'.
      wl_zhcme0001-hr_attrib_class = 'CL_EDUC_INST'.
      SELECT SINGLE hr_att_group_txt
      FROM t799battrib06ct INTO wl_zhcme0001-hr_att_group_txt
        WHERE hr_attrib_group EQ wl_zhcme0001-hr_attrib_group.

      SELECT SINGLE hr_attribute_txt
      FROM t799battrib01t INTO wl_zhcme0001-hr_attribute_txt
        WHERE hr_attribute EQ wl_zhcme0001-hr_attrib_class.
      wl_zhcme0001-molga = '37'.
      MOVE-CORRESPONDING wl_zhcme0001 TO p_registro_manter.


    WHEN 'ZHCMT0013'.
      wl_zhcme0002_aux-hr_attrib_class = 'CL_EDUCATION_ORG'.

      SELECT SINGLE hr_attribute_txt
      FROM t799battrib01t INTO wl_zhcme0002_aux-hr_attribute_txt
        WHERE hr_attribute EQ wl_zhcme0002_aux-hr_attrib_class.
      wl_zhcme0002_aux-hr_cvalue = '0001'.
      wl_zhcme0002_aux-molga = '37'.
      MOVE-CORRESPONDING wl_zhcme0002_aux TO p_registro_manter.

    WHEN 'ZHCMT0016'.
      wl_zhcme0002_aux-hr_attrib_class = 'CL_QUALIFIER_ENT'.

      SELECT SINGLE hr_attribute_txt
      FROM t799battrib01t INTO wl_zhcme0002_aux-hr_attribute_txt
        WHERE hr_attribute EQ wl_zhcme0002_aux-hr_attrib_class.
      wl_zhcme0002_aux-hr_cvalue = '0001'.
      wl_zhcme0002_aux-molga = '37'.
      MOVE-CORRESPONDING wl_zhcme0002_aux TO p_registro_manter.

    WHEN 'ZHCMT0017'.
      wl_zhcme0002_aux-hr_attrib_class = 'CL_COMP_ACTIVITY'.

      SELECT SINGLE hr_attribute_txt
      FROM t799battrib01t INTO wl_zhcme0002_aux-hr_attribute_txt
        WHERE hr_attribute EQ wl_zhcme0002_aux-hr_attrib_class.
      wl_zhcme0002_aux-hr_cvalue = '0001'.
      wl_zhcme0002_aux-molga = '37'.
      MOVE-CORRESPONDING wl_zhcme0002_aux TO p_registro_manter.

    WHEN OTHERS.
  ENDCASE.
ENDFORM.
