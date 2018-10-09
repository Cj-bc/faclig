#!/usr/bin/env python3
# code from https://qiita.com/mix_dvd/items/98feedc8c98bc7790b30


import cv2
import time
import shutil

if __name__ == '__main__':
    ESC_KEY = 27
    INTERVAL = 33
    FRAME_RATE = 30
    TERM_SIZE = shutil.get_terminal_size()

    ORG_WINDOW_NAME = "org"

    DEVICE_ID = 0

    cascade_file = "haarcascades/haarcascade_eye.xml"
    cascade = cv2.CascadeClassifier(cascade_file)

    cap = cv2.VideoCapture(DEVICE_ID)
    width = cap.get(cv2.CAP_PROP_FRAME_WIDTH)
    height = cap.get(cv2.CAP_PROP_FRAME_HEIGHT)

    end_flag, c_frame = cap.read()
    height, width, channels = c_frame.shape

    cv2.namedWindow(ORG_WINDOW_NAME)

    while end_flag == True:

        img = c_frame
        img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
        face_list = cascade.detectMultiScale(img_gray, minSize=(100, 100))

        # x: left > right
        # y: bottom > top
        for (x, y, w, h) in face_list:
            x_percent = round(x / width * 100)
            y_percent = round(y / height * 100)
            print(f'x:{x_percent}, y:{y_percent}')
            time.sleep(0.5)

        cv2.imshow(ORG_WINDOW_NAME, c_frame)

        key = cv2.waitKey(INTERVAL)
        if key == ESC_KEY:
            break

        end_flag, c_frame = cap.read()

    cv2.destroyAllWindows()
    cap.release()
