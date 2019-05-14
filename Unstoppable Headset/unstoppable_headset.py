import os
import sys
import pygame


def main():
    pygame.init()
    pygame.display.set_mode(pygame.display.list_modes()[-1])
    pygame.display.set_caption("Unstoppable Headset")

    pygame.mixer.init()
    sound = pygame.mixer.Sound(os.path.join(os.path.dirname(os.path.abspath(__file__)), "sound.wav"))
    sound.set_volume(0.3)
    sound.play(loops=-1)

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

    pygame.display.quit()
    pygame.quit()
    sys.exit()


if __name__ == "__main__":
    if os.fork():
        print('Detaching process...')
        sys.exit()

    main()

