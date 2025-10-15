*&---------------------------------------------------------------------*
*& Report ZTESTEJT0005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztestejt0005.

PARAMETERS: p_json TYPE string.

DATA: t_fardo   TYPE zpmt0059,
      obj_trace TYPE REF TO zcl_webservice_trace.

FREE: obj_trace.

CREATE OBJECT: obj_trace.

FREE: t_fardo.

CALL METHOD obj_trace->atualiza_trace
  EXPORTING
    t_fardo       = t_fardo               " Estrutura para retorno
    i_json        = p_json
    id_referencia = 'REENVIO_JOB'   "in_ref     " Id Referencia do LOG da Integração
  RECEIVING
    ret_code      = DATA(_ret_code).      " Status Code
