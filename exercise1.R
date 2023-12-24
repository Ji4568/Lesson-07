###�m��1�G�Ƿ|�Q�Φۭq��ƲզX�X�Q�n���ƾڮ榡
#�{�b�ЧQ�Φۭq��ƥ\��A�g�X��Ө�ơA�W��p�U�G
#��Ƥ�input�������@���O�ܶ��Τ@�s���ܶ�
#��Ƥ������]�w�@���ܶ��A���ϥΪ̯�M�w��ܤp���I�����
#3-1. �Ĥ@�Ө�ƥ�����X�b�����O�ܶ������P���O�ɡA�ӳs���ܶ����y�����ơӼзǮt�z
#3-2. �ĤG�Ө�ƥ�����X�b�����O�ܶ������P���O�ɡA�ӳs���ܶ����y�����(25�ʤ���-75�ʤ���)�z
#�`�N�G�ЦҼ{���O�Ƥ��u�O2�����ΡA�H�����O���بëD��0�B1�B2�B�K�����p

###�m��1����
#�o�O�Ĥ@�Ө�ơG
func.1 = function (x, y, dig = 3) {
  
  lvl.x = levels(factor(x))
  result = rep(NA, length(lvl.x))
  
  for (i in 1:length(lvl.x)) {
    m = mean(y[x==lvl.x[i]], na.rm = TRUE)
    s = sd(y[x==lvl.x[i]], na.rm = TRUE)
    result[i] = paste0(formatC(m, digits = dig, format = "f"), "��", formatC(s, digits = dig, format = "f"))
  }
  
  result
  
}

func.1(x = dat[,"Disease"], y = dat[,"eGFR"])

func.1(x = dat[,"Education"], y = dat[,"SBP"], dig = 1)

#�o�O�ĤG�Ө�ơG
func.2 = function (x, y, dig = 3) {
  
  lvl.x = levels(factor(x))
  result = rep(NA, length(lvl.x))
  
  for (i in 1:length(lvl.x)) {
    m = median(y[x==lvl.x[i]], na.rm = TRUE)
    q.25 = quantile(y[x==lvl.x[i]], 0.25, na.rm = TRUE)
    q.75 = quantile(y[x==lvl.x[i]], 0.75, na.rm = TRUE)
    result[i] = paste0(formatC(m, digits = dig, format = "f"), "(", formatC(q.25, digits = dig, format = "f"), "-", formatC(q.75, digits = dig, format = "f"), ")")
  }
  
  result
  
}

func.2(x = dat[,"Disease"], y = dat[,"eGFR"])

func.2(x = dat[,"Education"], y = dat[,"SBP"], dig = 1)
