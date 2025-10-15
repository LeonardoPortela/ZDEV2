**********************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.            *
*                                                                                *
**********************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                           *
* Data desenv ...: 18.05.2021                                                    *
* Objetivo    ...: Auditoria FB03                                                *
* Transação   ...: FB03                                                          *
**********************************************************************************

**********************************************************************************
* INCLUIR NOVAS INFORMACOES
**********************************************************************************
FORM f_tratar_informacoes TABLES t_bkpf_alv LIKE gt_ybkpf_alv.

  DATA: l_texto1    TYPE string,
        l_texto2    TYPE string,
        l_xdoc      TYPE char10,
        l_achou     TYPE c,
        l_interface TYPE zib_contabil-interface,
        l_tabix     TYPE sy-tabix.

  CLEAR l_achou.

  LOOP AT t_bkpf_alv INTO DATA(w_bkpf_alv).

    l_tabix = sy-tabix.

*--------------------------------
*-- regra 1
*--------------------------------
    IF w_bkpf_alv-awkey(5) = 'ZGL17'.
      w_bkpf_alv-transac_exec = 'ZGL016'.
      SPLIT w_bkpf_alv-bktxt AT '-' INTO l_texto1 l_texto2.
      w_bkpf_alv-usuario_exec = l_texto1.

      PERFORM f_busca_departamento USING w_bkpf_alv-usnam
                                CHANGING w_bkpf_alv-departa_exec.

      MODIFY t_bkpf_alv FROM w_bkpf_alv INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------
*-- regra 2
*--------------------------------
    IF w_bkpf_alv-tcode <> 'FB05'.
      w_bkpf_alv-transac_exec = w_bkpf_alv-tcode.
      w_bkpf_alv-usuario_exec = w_bkpf_alv-usnam.

      PERFORM f_busca_departamento USING w_bkpf_alv-usnam
                                CHANGING w_bkpf_alv-departa_exec.

      MODIFY t_bkpf_alv FROM w_bkpf_alv INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------
*-- regra 3
*--------------------------------
    IF w_bkpf_alv-awkey(2) = 'ZP'.
      w_bkpf_alv-transac_exec = 'ZIMP53'.

      l_xdoc = w_bkpf_alv-awkey+6(10).

      SELECT usuario
        INTO w_bkpf_alv-usuario_exec
        FROM zimp_lanc_impost
          UP TO 1 ROWS
       WHERE doc_imposto = l_xdoc
         AND bukrs       = w_bkpf_alv-bukrs.
      ENDSELECT.

      PERFORM f_busca_departamento USING w_bkpf_alv-usnam
                                CHANGING w_bkpf_alv-departa_exec.

      MODIFY t_bkpf_alv FROM w_bkpf_alv INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------
*-- regra 4
*--------------------------------
    IF w_bkpf_alv-awkey(2) = 'ZG'.
      w_bkpf_alv-transac_exec = 'ZNFW0005'.
      w_bkpf_alv-usuario_exec = w_bkpf_alv-usnam.

      PERFORM f_busca_departamento USING w_bkpf_alv-usnam
                                CHANGING w_bkpf_alv-departa_exec.

      MODIFY t_bkpf_alv FROM w_bkpf_alv INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------
*-- regra 5
*--------------------------------
    IF w_bkpf_alv-tcode = 'FB05' AND w_bkpf_alv-awtyp <> 'IDOC'.
      SELECT usnam
        INTO w_bkpf_alv-usuario_exec
        FROM zfit0122
          UP TO 1 ROWS
       WHERE bukrs = w_bkpf_alv-bukrs
         AND belnr = w_bkpf_alv-belnr
         AND gjahr = w_bkpf_alv-gjahr.
      ENDSELECT.

      IF sy-subrc = 0.
        w_bkpf_alv-transac_exec = 'ZFI0105'.
      ELSE.
        w_bkpf_alv-transac_exec = 'F-51'.
      ENDIF.

      w_bkpf_alv-usuario_exec   = w_bkpf_alv-usnam.

      PERFORM f_busca_departamento USING w_bkpf_alv-usnam
                                CHANGING w_bkpf_alv-departa_exec.

      MODIFY t_bkpf_alv FROM w_bkpf_alv INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------
*-- regra 6
*--------------------------------
    IF ( w_bkpf_alv-tcode = 'FB05'    OR
         w_bkpf_alv-tcode = 'FB08' ) AND
       ( w_bkpf_alv-blart = 'ND'      OR
         w_bkpf_alv-blart = 'NC'      OR
         w_bkpf_alv-blart = 'NL'      OR
         w_bkpf_alv-blart = 'NM' )   AND
         w_bkpf_alv-awtyp = 'IDOC'.

      w_bkpf_alv-transac_exec = 'ZFIS26'.

      SELECT uname
        INTO w_bkpf_alv-usuario_exec
        FROM zfit0026
          UP TO 1 ROWS
       WHERE docnum = w_bkpf_alv-belnr.
      ENDSELECT.

      IF sy-subrc <> 0.
        w_bkpf_alv-usuario_exec   = w_bkpf_alv-usnam.
      ENDIF.

      PERFORM f_busca_departamento USING w_bkpf_alv-usnam
                                CHANGING w_bkpf_alv-departa_exec.

      MODIFY t_bkpf_alv FROM w_bkpf_alv INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------
*-- regra 7
*--------------------------------
    IF w_bkpf_alv-tcode = 'FB05' AND
       w_bkpf_alv-awtyp = 'IDOC'.

      CLEAR l_interface.
      SELECT interface
        INTO l_interface
        FROM zib_contabil
          UP TO 1 ROWS
       WHERE obj_key = w_bkpf_alv-awkey.
      ENDSELECT.

      IF     l_interface = '03' OR  l_interface = '11'  OR l_interface = '01' OR
             l_interface = '04' OR  l_interface = '47'  OR l_interface = '54'.
        w_bkpf_alv-transac_exec = 'Interface SIGAM'.
      ELSEIF l_interface = '43'.
        w_bkpf_alv-transac_exec = 'SE-Controle Viagem'.
      ENDIF.

      w_bkpf_alv-usuario_exec   = w_bkpf_alv-usnam.

      PERFORM f_busca_departamento USING w_bkpf_alv-usnam
                                CHANGING w_bkpf_alv-departa_exec.

      MODIFY t_bkpf_alv FROM w_bkpf_alv INDEX l_tabix.
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.

**********************************************************************************
* busca departamento
**********************************************************************************
FORM f_busca_departamento USING p_usnam
                       CHANGING p_depart.

  FREE: p_depart.

*--------------------------------
*-- departamento
*--------------------------------
  SELECT department
    INTO p_depart
    FROM user_addr
      UP TO 1 ROWS
   WHERE bname = p_usnam.
  ENDSELECT.

ENDFORM.

**********************************************************************************
* ADICIONAR CAMPOS
**********************************************************************************
FORM f_add_fieldcat       TABLES t_fieldcat LIKE gt_fieldcat.

  DATA: t_fields   TYPE slis_t_fieldcat_alv,
        w_fields   TYPE slis_fieldcat_alv,
        w_fieldcat TYPE slis_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_buffer_active        = abap_false           " Note 542413
      i_structure_name       = con_alv_struc
    CHANGING
      ct_fieldcat            = t_fields
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT t_fields INTO w_fields.
    READ TABLE t_fieldcat INTO w_fieldcat WITH KEY fieldname = w_fields-fieldname.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING w_fields  TO w_fieldcat.
      APPEND w_fieldcat            TO t_fieldcat.
    ENDIF.
  ENDLOOP.

ENDFORM.

**********************************************************************************
**********************************************************************************
