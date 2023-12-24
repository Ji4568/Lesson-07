###�Ĥ@�`�G�y�z�ʲέp(1)
#�o�`�ҧڭ̲ש�n�}�l�i��ƾڤ��R�F�A�b�̶}�l�ڭ̭n²�椶�дX�Ӵy�z�ʲέp�����
#�ФU�����g���d�Ҹ�� ��Example data.csv��
dat = read.csv("Example data.csv", header = TRUE, fileEncoding = 'CP950')
head(dat)

#�H�U�X�Ө�Ƭ��`�����y�z�ʲέp�A���O�O�G
#��ơumean()�v�i�H���U�ڭ̭p�⥭����
mean(dat$eGFR, na.rm = TRUE)

#��ơusd()�v�i�H���U�ڭ̭p��зǮt
sd(dat$eGFR, na.rm = TRUE)

#��ơuvar()�v�i�H���U�ڭ̭p���ܲ���
var(dat$eGFR, na.rm = TRUE)

#��ơumedian()�v�i�H���U�ڭ̭p���ܲ���
median(dat$eGFR, na.rm = TRUE)

#��ơuquantile()�v�i�H���U�ڭ̭p��ʤ����
quantile(dat$eGFR, na.rm = TRUE)

quantile(dat$eGFR, 0.5, na.rm = TRUE)

quantile(dat$eGFR, 0.95, na.rm = TRUE)

#��ơumin()�v�i�H���U�ڭ̧�X�̤p��
min(dat$eGFR, na.rm = TRUE)

#��ơumax()�v�i�H���U�ڭ̧�X�̤j��
max(dat$eGFR, na.rm = TRUE)

#��ơutable()�v�i�H���U�ڭ̲��ͦC�p��
table(dat$Cancer)

table(dat$Cancer, dat$Diabetes)

#��ơuprop.table()�v�i�H���U�ڭ̲��ͦC�p�����ʤ���
tab1 = table(dat$Cancer)
prop.table(tab1)

tab2 = table(dat$Cancer, dat$Diabetes)
prop.table(tab2)

prop.table(tab2, 1)

prop.table(tab2, 2)