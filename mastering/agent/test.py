import base64
import sys

def display_image(image_path):
    with open(image_path, 'rb') as f:
        image_data=base64.b64encode(f.read()).decode('ascii')

    pc = f'\033_Ga=T,f=100;{image_data}\033\\'
    sys.stdout.buffer.write(pc.encode('ascii'))
    sys.stdout.buffer.flush()

display_image("/tmp/qrcode.png")
