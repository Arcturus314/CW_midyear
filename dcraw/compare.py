fexp = open('after.txt','r')
fout = open('test_top_out.txt','r')

line_exp = fexp.readline()
line_out = fout.readline()
pixel_cnt = 0
num_col = 5218
while ((line_exp != '') and (line_out != '')):
    row = pixel_cnt / num_col
    col = pixel_cnt % num_col
    pixels_exp = line_exp.split('_')
    pixels_out = line_out.split('_')
    if (abs(int(pixels_exp[0],16)- int(pixels_out[0],16))>2): print('Row '+ str(row) + ' Col ' + str(col) + ' Red exp: '+ str(pixels_exp[0]) + ' out: '+str(pixels_out[0]))
    if (abs(int(pixels_exp[1],16)- int(pixels_out[1],16))>2): print('Row '+ str(row) + ' Col ' + str(col) + ' Green exp: '+ str(pixels_exp[1]) + ' out: '+str(pixels_out[1]))
    if (abs(int(pixels_exp[2],16)- int(pixels_out[2],16))>2): print('Row '+ str(row) + ' Col ' + str(col) + ' Blue exp: '+ str(pixels_exp[2]) + ' out: '+str(pixels_out[2]))
    pixel_cnt = pixel_cnt + 1
    line_exp = fexp.readline()
    line_out = fout.readline()
